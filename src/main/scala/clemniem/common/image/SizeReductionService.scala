package clemniem.common.image

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
    if (w <= targetMaxW && h <= targetMaxH) image.copy()
    else {
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
    }
    }
  }

  private def downscaleAverage(src: RawImage, nw: Int, nh: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val out = RawImage.create(nw, nh)
    for (dy <- 0 until nh; dx <- 0 until nw) {
      val x0 = (dx * w) / nw
      val y0 = (dy * h) / nh
      val x1 = ((dx + 1) * w).min(w)
      val y1 = ((dy + 1) * h).min(h)
      val (r, g, b, a, n) = (y0 until y1).flatMap(sy => (x0 until x1).map(sx => (sy, sx))).foldLeft((0L, 0L, 0L, 0L, 0L)) { case ((r0, g0, b0, a0, n0), (sy, sx)) =>
        val i = (sy * w + sx) * 4
        (r0 + (src.data(i) & 0xff), g0 + (src.data(i + 1) & 0xff), b0 + (src.data(i + 2) & 0xff), a0 + (src.data(i + 3) & 0xff), n0 + 1)
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
        val v = recurse(half, x % half, y % half)
        val add = (if ((y / half) % 2 == 0) 0 else 2) + (if ((x / half) % 2 == 0) 0 else 1)
        (v * 4 + add) / 4.0
      }
    recurse(matrixSize, x % matrixSize, y % matrixSize)
  }

  private def downscaleBayer(src: RawImage, nw: Int, nh: Int, matrixSize: Int): RawImage = {
    val w   = src.width
    val h   = src.height
    val out = RawImage.create(nw, nh)
    for (dy <- 0 until nh; dx <- 0 until nw) {
      val x0     = (dx * w) / nw
      val y0     = (dy * h) / nh
      val thresh = bayerValue(matrixSize, dx, dy)
      val bw     = ((dx + 1) * w) / nw - x0
      val bh     = ((dy + 1) * h) / nh - y0
      val total  = bw * bh
      val pick   = (thresh * (total - 1)).toInt.max(0).min(total - 1)
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
