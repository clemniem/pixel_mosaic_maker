package clemniem.common.image

import clemniem.common.PixelArtDetection

import scala.collection.mutable.ArrayBuffer

/** Strategy for choosing the value of each output pixel when downscaling (one pixel per block). */
sealed trait DownscaleStrategy {
  def name: String
}

/** Average all source pixels in each block (smooth result). */
case object DownscaleAverage extends DownscaleStrategy {
  override def name: String = "Smooth (average)"
}

/** Pick one source pixel per block using a Bayer matrix (Game Boy Camera–style dithered look). */
final case class DownscaleBayer(matrixSize: Int) extends DownscaleStrategy {
  override def name: String = s"Dithered (${matrixSize}×${matrixSize} Bayer)"
}

object DownscaleBayer {
  val Size2: DownscaleBayer = DownscaleBayer(2)
  val Size4: DownscaleBayer = DownscaleBayer(4)
}

/** Pure nearest-neighbor downscale that preserves the original color set. When the source is an integer
  * nearest-neighbor upscale (e.g. a 4x scaled GB Camera image), the original pixels are recovered exactly.
  */
case object DownscalePixelPerfect extends DownscaleStrategy {
  override def name: String = "Pixel-perfect (nearest)"
}

/** Standalone service: validate upload size, downscale image to target max dimensions. */
object SizeReductionService {

  val MaxUploadWidth: Int  = 5000
  val MaxUploadHeight: Int = 5000
  val TargetMaxWidth: Int  = 500
  val TargetMaxHeight: Int = 500

  def exceedsMaxUpload(width: Int, height: Int): Boolean =
    width > MaxUploadWidth || height > MaxUploadHeight

  /** Downscale so that width ≤ targetW and height ≤ targetH. Uses block sampling. */
  def downscale(
    image: RawImage,
    targetMaxW: Int,
    targetMaxH: Int,
    strategy: DownscaleStrategy
  ): RawImage = {
    val w = image.width
    val h = image.height
    strategy match {
      case DownscalePixelPerfect =>
        downscalePixelPerfect(image, targetMaxW, targetMaxH)
      case _ if w <= targetMaxW && h <= targetMaxH =>
        image.copy
      case _ =>
        val scaleX = w.toDouble / targetMaxW
        val scaleY = h.toDouble / targetMaxH
        val scale  = math.max(scaleX, scaleY)
        val nw     = (w / scale).toInt.max(1).min(targetMaxW)
        val nh     = (h / scale).toInt.max(1).min(targetMaxH)
        strategy match {
          case DownscaleAverage =>
            downscaleAverage(image, nw, nh)
          case b: DownscaleBayer =>
            downscaleBayer(image, nw, nh, b.matrixSize)
          case DownscalePixelPerfect =>
            downscaleModeFilter(image, nw, nh)
        }
    }
  }

  private val PixelPerfectTolerance = 8

  /** Native Game Boy Camera image sizes:
    *   - 128×112: raw sensor output, no UI frame.
    *   - 160×144: with the standard Game Boy LCD frame applied (this is also the Game Boy screen resolution).
    * Almost every user upload originates from one of these (often integer-NN-upscaled by emulators or export tools),
    * so we treat a clean integer multiple of either size as a definitive answer that bypasses pixel-level detection.
    */
  private val GbCameraNativeSizes: List[(Int, Int)] = List((128, 112), (160, 144))

  /** Return Some(F) (F in 1..10) iff `(w, h)` equals (bw*F, bh*F) for some Game Boy Camera native size `(bw, bh)`. */
  private def gbCameraFactor(w: Int, h: Int): Option[Int] =
    GbCameraNativeSizes.iterator
      .flatMap { case (bw, bh) =>
        if (w % bw == 0 && h % bh == 0 && (w / bw) == (h / bh)) Some(w / bw) else None
      }
      .find(f => f >= 1 && f <= 10)

  /** Game Boy Camera images are guaranteed to have a 4-colour palette. When we recognise the dimensions, we lock the
    * output to exactly 4 colours by deriving the palette from the source (median cut + k-means refinement) and
    * snapping every output block to the nearest of those 4. Sidesteps any byte-level canvas/JPEG noise that would
    * otherwise split a single logical colour across multiple buckets.
    */
  private val GbCameraPaletteSize = 4

  /** Pixel-perfect strategy:
    *   1. If the dimensions are an integer multiple of a Game Boy Camera native size (`gbCameraFactor`), use the
    *      palette-snap path that locks the output to exactly 4 colours.
    *   2. Otherwise fall back to tolerant integer-factor detection + canonical-bucket mode filter (which absorbs
    *      ±8 byte noise but does not enforce a fixed palette size).
    */
  private def downscalePixelPerfect(src: RawImage, targetMaxW: Int, targetMaxH: Int): RawImage = {
    val w = src.width
    val h = src.height
    gbCameraFactor(w, h) match {
      case Some(1) =>
        src.copy
      case Some(f) if (w / f) <= targetMaxW && (h / f) <= targetMaxH =>
        downscalePixelPerfectGb(src, f)
      case _ =>
        val detectedFactor =
          PixelArtDetection.detectNearestNeighborScaleFromBytes(w, h, src.data, PixelPerfectTolerance)
        val factor =
          detectedFactor.filter(f => f >= 2 && (w / f) <= targetMaxW && (h / f) <= targetMaxH)
        factor match {
          case Some(f) =>
            downscaleModeFilter(src, w / f, h / f)
          case None if w <= targetMaxW && h <= targetMaxH =>
            src.copy
          case None =>
            val scaleX = w.toDouble / targetMaxW
            val scaleY = h.toDouble / targetMaxH
            val scale  = math.max(scaleX, scaleY)
            val nw     = (w / scale).toInt.max(1).min(targetMaxW)
            val nh     = (h / scale).toInt.max(1).min(targetMaxH)
            downscaleModeFilter(src, nw, nh)
        }
    }
  }

  /** GB-Camera-specific downscale: derive a 4-colour palette from the source, then for each FxF source block compute
    * its average colour and snap to the nearest palette entry. Guarantees exactly 4 (or fewer) unique output colours.
    *
    * For a perfectly clean GB Camera PNG, the average of each FxF block equals one of the 4 palette colours exactly,
    * so the snap is a no-op and output bytes match the source. For a noisy upload (sRGB roundtrip, light JPEG,
    * re-encoded screenshots), the average denoises the block and the snap collapses the result to the closest of the
    * 4 dominant colours that median cut + k-means identified — exactly matching the user's request to "re-create the
    * image by jumping from bucket to bucket and re-assigning it the colour closest from the 4 original palettes".
    */
  private def downscalePixelPerfectGb(src: RawImage, factor: Int): RawImage = {
    val w       = src.width
    val h       = src.height
    val nw      = w / factor
    val nh      = h / factor
    val out     = RawImage.create(nw, nh)
    val palette = ColorQuantizationService.medianCutPalette(src, GbCameraPaletteSize)

    for {
      dy <- 0 until nh
      dx <- 0 until nw
    } {
      val x0 = dx * factor
      val y0 = dy * factor
      val x1 = (dx + 1) * factor
      val y1 = (dy + 1) * factor

      val (sr, sg, sb, n) =
        (y0 until y1).flatMap(sy => (x0 until x1).map(sx => (sy, sx))).foldLeft((0L, 0L, 0L, 0L)) {
          case ((rr, gg, bb, nn), (sy, sx)) =>
            val si = (sy * w + sx) * 4
            (
              rr + (src.data(si) & 0xff),
              gg + (src.data(si + 1) & 0xff),
              bb + (src.data(si + 2) & 0xff),
              nn + 1)
        }
      val avgR = (sr / n).toInt
      val avgG = (sg / n).toInt
      val avgB = (sb / n).toInt

      val bestIdx = palette.indices.minBy { i =>
        val (pr, pg, pb, _) = palette(i)
        val dr              = avgR - (pr & 0xff)
        val dg              = avgG - (pg & 0xff)
        val db              = avgB - (pb & 0xff)
        dr * dr + dg * dg + db * db
      }
      val (pr, pg, pb, pa) = palette(bestIdx)
      val o                = (dy * nw + dx) * 4
      out.data(o) = pr
      out.data(o + 1) = pg
      out.data(o + 2) = pb
      out.data(o + 3) = pa
    }
    out
  }

  /** Mode-filter (majority-colour) downscale. For each output cell the most frequent colour bucket in the source block
    * wins, and the bucket is materialised as a **globally consistent canonical pixel** — the first source pixel ever
    * seen in that bucket. Two cells that pick the same bucket therefore emit byte-identical output, which is critical
    * for the downstream `countUniqueColors` heuristic that drives the auto palette size in the upload UI.
    *
    * Without the canonical pass, two cells could both pick "bucket = black" but emit `(0,0,0)` vs `(0,0,1)` (depending
    * on which noisy source pixel they happened to scan first), so a 4-colour source ends up looking like an 8-colour
    * image and the auto-quantizer over-splits — the user sees e.g. "5 nearly-identical blacks".
    *
    * Bucket key: 4 bits per channel (R, G, B) → 4096 buckets of size 16. This absorbs ±8 canvas sRGB noise while
    * still distinguishing palette colours that differ by ≥16 in any channel.
    *
    * Implementation is scalafix-clean: no var/while/return/null. Two passes:
    *   1. Single linear scan to record the first source pixel index per bucket (`canonicalSi`).
    *   2. Standard per-cell histogram → pick winning bucket → emit canonical bytes.
    *
    * Both passes are O(W·H). The per-cell histogram array is pre-allocated once and zeroed lazily via a touched-index
    * list.
    */
  private def downscaleModeFilter(src: RawImage, nw: Int, nh: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val out = RawImage.create(nw, nh)

    // Pass 1: record the first source-pixel index that lands in each bucket. -1 means "bucket unused".
    val canonicalSi = Array.fill(4096)(-1)
    val totalPx     = w * h
    for (i <- 0 until totalPx) {
      val si  = i * 4
      val r   = src.data(si) & 0xff
      val g   = src.data(si + 1) & 0xff
      val b   = src.data(si + 2) & 0xff
      val key = ((r >> 4) << 8) | ((g >> 4) << 4) | (b >> 4)
      if (canonicalSi(key) < 0) canonicalSi(key) = si
    }

    // Pass 2: per-cell mode → output canonical bytes.
    val hist    = new Array[Int](4096)
    val touched = ArrayBuffer.empty[Int]
    for {
      dy <- 0 until nh
      dx <- 0 until nw
    } {
      val x0 = (dx * w) / nw
      val y0 = (dy * h) / nh
      val x1 = (((dx + 1) * w) / nw).min(w)
      val y1 = (((dy + 1) * h) / nh).min(h)

      touched.clear()
      for {
        sy <- y0 until y1
        sx <- x0 until x1
      } {
        val si  = (sy * w + sx) * 4
        val r   = src.data(si) & 0xff
        val g   = src.data(si + 1) & 0xff
        val b   = src.data(si + 2) & 0xff
        val key = ((r >> 4) << 8) | ((g >> 4) << 4) | (b >> 4)
        if (hist(key) == 0) touched += key
        hist(key) += 1
      }

      val bestKey = touched.maxBy(k => hist(k))
      val csi     = canonicalSi(bestKey)
      val o       = (dy * nw + dx) * 4
      out.data(o) = src.data(csi)
      out.data(o + 1) = src.data(csi + 1)
      out.data(o + 2) = src.data(csi + 2)
      out.data(o + 3) = src.data(csi + 3)

      touched.foreach(k => hist(k) = 0)
    }
    out
  }

  private def downscaleAverage(src: RawImage, nw: Int, nh: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val out = RawImage.create(nw, nh)
    for {
      dy <- 0 until nh
      dx <- 0 until nw
    } {
      val x0 = (dx * w) / nw
      val y0 = (dy * h) / nh
      val x1 = ((dx + 1) * w).min(w)
      val y1 = ((dy + 1) * h).min(h)
      val (r, g, b, a, n) =
        (y0 until y1).flatMap(sy => (x0 until x1).map(sx => (sy, sx))).foldLeft((0L, 0L, 0L, 0L, 0L)) {
          case ((r0, g0, b0, a0, n0), (sy, sx)) =>
            val i = (sy * w + sx) * 4
            (
              r0 + (src.data(i) & 0xff),
              g0 + (src.data(i + 1) & 0xff),
              b0 + (src.data(i + 2) & 0xff),
              a0 + (src.data(i + 3) & 0xff),
              n0 + 1)
        }
      if (n > 0) {
        val o = (dy * nw + dx) * 4
        out.data(o) = (r / n).toByte
        out.data(o + 1) = (g / n).toByte
        out.data(o + 2) = (b / n).toByte
        out.data(o + 3) = (a / n).toByte
      }
    }
    out
  }

  private def bayerValue(matrixSize: Int, x: Int, y: Int): Double = {
    def recurse(n: Int, x: Int, y: Int): Double =
      if (n <= 1) 0.5
      else {
        val half = n / 2
        val v    = recurse(half, x % half, y % half)
        val add  = (if ((y / half) % 2 == 0) 0 else 2) + (if ((x / half) % 2 == 0) 0 else 1)
        (v * 4 + add) / 4.0
      }
    recurse(matrixSize, x % matrixSize, y % matrixSize)
  }

  private def downscaleBayer(src: RawImage, nw: Int, nh: Int, matrixSize: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val out = RawImage.create(nw, nh)
    for {
      dy <- 0 until nh
      dx <- 0 until nw
    } {
      val x0          = (dx * w) / nw
      val y0          = (dy * h) / nh
      val thresh      = bayerValue(matrixSize, dx, dy)
      val bw          = ((dx + 1) * w) / nw - x0
      val bh          = ((dy + 1) * h) / nh - y0
      val total       = bw * bh
      val pick        = (thresh * (total - 1)).toInt.max(0).min(total - 1)
      val linearIndex = (y0 until y0 + bh).flatMap(sy => (x0 until x0 + bw).map(sx => (sy, sx))).lift(pick)
      linearIndex match {
        case Some((sy, sx)) =>
          val si = (sy * w + sx) * 4
          val o  = (dy * nw + dx) * 4
          out.data(o) = src.data(si)
          out.data(o + 1) = src.data(si + 1)
          out.data(o + 2) = src.data(si + 2)
          out.data(o + 3) = src.data(si + 3)
        case None =>
          val si = (y0 * w + x0) * 4
          val o  = (dy * nw + dx) * 4
          out.data(o) = src.data(si)
          out.data(o + 1) = src.data(si + 1)
          out.data(o + 2) = src.data(si + 2)
          out.data(o + 3) = src.data(si + 3)
      }
    }
    out
  }
}
