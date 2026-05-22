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

  /** Upper bound on unique colours treated as "pixel art / GB Camera" for the plain-NN gate. */
  private val GbCameraPaletteSize = 4

  /** Pixel-perfect strategy:
    *   1. If the dimensions are factor-1 of a GB Camera native size, return `src.copy` (already at native resolution).
    *   2. If the dimensions are an integer multiple F ≥ 2 of a GB Camera native size **and** the source has ≤ 4
    *      unique colours, use plain stride-F nearest-neighbour sampling. Source bytes are preserved exactly — no
    *      averaging, no snap, no palette derivation. Multicoloured / HDR images at GB dimensions fall through.
    *   3. Otherwise: tolerant integer-factor detection + canonical-bucket mode filter.
    */
  private def downscalePixelPerfect(src: RawImage, targetMaxW: Int, targetMaxH: Int): RawImage = {
    val w = src.width
    val h = src.height
    gbCameraFactor(w, h) match {
      case Some(1) =>
        src.copy
      case Some(f)
          if (w / f) <= targetMaxW && (h / f) <= targetMaxH
            && hasAtMostUniqueColors(src, GbCameraPaletteSize) =>
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

  /** True iff `src` has at most `max` distinct RGBA colours. Short-circuits on the first pixel that would push the
    * unique count above `max`, so it is O(1) for non-matching images and only O(W·H) for matching ones.
    */
  private[common] def hasAtMostUniqueColors(src: RawImage, max: Int): Boolean = {
    val data = src.data
    val n    = src.pixelCount
    val seen = scala.collection.mutable.HashSet.empty[Long]
    !(0 until n).iterator.exists { i =>
      val o   = i * 4
      val key =
        ((data(o) & 0xff).toLong << 24) |
          ((data(o + 1) & 0xff).toLong << 16) |
          ((data(o + 2) & 0xff).toLong << 8) |
          (data(o + 3) & 0xff).toLong
      seen += key
      seen.size > max
    }
  }

  /** Plain integer-stride nearest-neighbour downscale for confirmed GB Camera pixel art (≤ 4 unique colours,
    * dimensions = N × native GB size). Picks the top-left pixel of each F×F block. Source bytes are copied verbatim —
    * no colour mutation of any kind.
    */
  private def downscalePixelPerfectGb(src: RawImage, factor: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val nw  = w / factor
    val nh  = h / factor
    val out = RawImage.create(nw, nh)
    for {
      dy <- 0 until nh
      dx <- 0 until nw
    } {
      val sy = dy * factor
      val sx = dx * factor
      val si = (sy * w + sx) * 4
      val o  = (dy * nw + dx) * 4
      out.data(o)     = src.data(si)
      out.data(o + 1) = src.data(si + 1)
      out.data(o + 2) = src.data(si + 2)
      out.data(o + 3) = src.data(si + 3)
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
