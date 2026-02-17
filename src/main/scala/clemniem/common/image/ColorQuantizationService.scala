package clemniem.common.image

import scala.collection.mutable.ArrayBuffer

/** Standalone service: reduce image to a fixed number of colors (4–16) with optional dithering. */
object ColorQuantizationService {

  val MinColors: Int = 4
  val MaxColors: Int = 16

  def clampNumColors(n: Int): Int = n.max(MinColors).min(MaxColors)

  /** Build palette from image using median cut, then map pixels with the given dithering. */
  def quantize(
    image: RawImage,
    numColors: Int,
    dithering: ColorDithering
  ): QuantizedResult = {
    val n       = clampNumColors(numColors)
    val palette = medianCutPalette(image, n)
    val indices = dithering.quantizeToIndices(image, palette)
    QuantizedResult(palette, indices)
  }

  private val KMeansIterations = 4

  /** Median cut: partition color space by channel range, split at median, repeat until we have n boxes; average each
    * box, then refine with a few k-means iterations to sharpen palette colors.
    */
  def medianCutPalette(image: RawImage, numColors: Int): Vector[(Byte, Byte, Byte, Byte)] = {
    val w = image.width
    val h = image.height
    val pixels = (0 until (w * h)).map { i =>
      val o = i * 4
      val r = image.data(o) & 0xff
      val g = image.data(o + 1) & 0xff
      val b = image.data(o + 2) & 0xff
      val a = image.data(o + 3) & 0xff
      (r, g, b, a)
    }.toVector
    if (pixels.isEmpty) Vector.fill(numColors)((0.toByte, 0.toByte, 0.toByte, 255.toByte))
    else {
      val boxes   = medianCutIterative(pixels, numColors)
      val initial = boxes.map(avgColor).toVector
      kMeansRefine(pixels, initial, KMeansIterations)
    }
  }

  /** Refine a palette with k-means: assign every pixel to nearest color, recompute centroids. */
  private def kMeansRefine(
    pixels: Vector[(Int, Int, Int, Int)],
    palette: Vector[(Byte, Byte, Byte, Byte)],
    iterations: Int
  ): Vector[(Byte, Byte, Byte, Byte)] =
    (0 until iterations).foldLeft(palette) { (current, _) =>
      val k = current.size
      // Accumulate sums per cluster: (sumR, sumG, sumB, sumA, count)
      val clusters = pixels.foldLeft(Vector.fill(k)((0L, 0L, 0L, 0L, 0L))) { case (acc, (r, g, b, a)) =>
        val idx                  = nearestIndex(r, g, b, current)
        val (sr, sg, sb, sa, sc) = acc(idx)
        acc.updated(idx, (sr + r, sg + g, sb + b, sa + a, sc + 1))
      }
      clusters.zipWithIndex.map { case ((sr, sg, sb, sa, sc), i) =>
        if (sc == 0) current(i)
        else
          (
            (sr / sc).toInt.max(0).min(255).toByte,
            (sg / sc).toInt.max(0).min(255).toByte,
            (sb / sc).toInt.max(0).min(255).toByte,
            (sa / sc).toInt.max(0).min(255).toByte
          )
      }
    }

  /** Find nearest palette index using perceptual weighted distance (no alpha). */
  private def nearestIndex(r: Int, g: Int, b: Int, palette: Vector[(Byte, Byte, Byte, Byte)]): Int =
    if (palette.isEmpty) 0
    else
      palette.indices.minBy { idx =>
        val (pr, pg, pb, _) = palette(idx)
        val dr              = r - (pr & 0xff)
        val dg              = g - (pg & 0xff)
        val db              = b - (pb & 0xff)
        2 * dr * dr + 4 * dg * dg + 3 * db * db
      }

  private def avgColor(pixels: Vector[(Int, Int, Int, Int)]): (Byte, Byte, Byte, Byte) =
    if (pixels.isEmpty) (0.toByte, 0.toByte, 0.toByte, 255.toByte)
    else {
      val n = pixels.size
      val (r, g, b, a) = pixels.foldLeft((0L, 0L, 0L, 0L)) { case ((r0, g0, b0, a0), (rr, gg, bb, aa)) =>
        (r0 + rr, g0 + gg, b0 + bb, a0 + aa)
      }
      (
        (r / n).toInt.max(0).min(255).toByte,
        (g / n).toInt.max(0).min(255).toByte,
        (b / n).toInt.max(0).min(255).toByte,
        (a / n).toInt.max(0).min(255).toByte
      )
    }

  private def boxRange(pixels: Vector[(Int, Int, Int, Int)]): (Int, Int) =
    if (pixels.size <= 1) (0, 0)
    else {
      val rMin   = pixels.map(_._1).min
      val rMax   = pixels.map(_._1).max
      val gMin   = pixels.map(_._2).min
      val gMax   = pixels.map(_._2).max
      val bMin   = pixels.map(_._3).min
      val bMax   = pixels.map(_._3).max
      val rangeR = rMax - rMin
      val rangeG = gMax - gMin
      val rangeB = bMax - bMin
      (
        List(0, 1, 2).maxBy(c => if (c == 0) rangeR else if (c == 1) rangeG else rangeB),
        math.max(rangeR, math.max(rangeG, rangeB)))
    }

  private def medianCutIterative(
    pixels: Vector[(Int, Int, Int, Int)],
    numColors: Int
  ): Vector[Vector[(Int, Int, Int, Int)]] = {
    def step(buf: ArrayBuffer[Vector[(Int, Int, Int, Int)]]): Vector[Vector[(Int, Int, Int, Int)]] =
      if (buf.size >= numColors) buf.take(numColors).toVector
      else {
        val (bestIdx, bestChannel, bestRange) = buf.indices
          .flatMap { idx =>
            val (ch, r) = boxRange(buf(idx))
            if (buf(idx).size > 1) Some((idx, ch, r)) else None
          }
          .maxByOption(_._3)
          .getOrElse((0, 0, -1))
        if (bestRange <= 0) buf.toVector
        else {
          val box    = buf(bestIdx)
          val sorted = box.sortBy { case (r, g, b, _) => if (bestChannel == 0) r else if (bestChannel == 1) g else b }
          val mid    = sorted.size / 2
          buf(bestIdx) = sorted.take(mid)
          buf += sorted.drop(mid)
          step(buf)
        }
      }
    step(ArrayBuffer(pixels))
  }
}
