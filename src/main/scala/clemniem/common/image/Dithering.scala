package clemniem.common.image

/** Result of color quantization: palette (RGBA bytes) and one index per pixel. */
final case class QuantizedResult(palette: Vector[(Byte, Byte, Byte, Byte)], indices: Array[Int])

/** Dithering algorithm for mapping image pixels to a fixed palette. Extensible (e.g. add different error matrices). */
trait ColorDithering {
  def name: String
  /** Produce one palette index per pixel (same order as RawImage: row-major). */
  def quantizeToIndices(image: RawImage, palette: Vector[(Byte, Byte, Byte, Byte)]): Array[Int]
}

/** No dithering: each pixel mapped to nearest palette color. */
object NoColorDithering extends ColorDithering {
  override def name: String = "None"
  override def quantizeToIndices(image: RawImage, palette: Vector[(Byte, Byte, Byte, Byte)]): Array[Int] = {
    val out = new Array[Int](image.pixelCount)
    val n   = image.width * image.height
    for (i <- 0 until n) {
      val o = i * 4
      val r = image.data(o) & 0xff
      val g = image.data(o + 1) & 0xff
      val b = image.data(o + 2) & 0xff
      val a = image.data(o + 3) & 0xff
      out(i) = nearestPaletteIndex(r, g, b, a, palette)
    }
    out
  }

  def nearestPaletteIndex(r: Int, g: Int, b: Int, a: Int, palette: Vector[(Byte, Byte, Byte, Byte)]): Int = {
    if (palette.isEmpty) 0
    else palette.indices.minBy { idx =>
      val (pr, pg, pb, pa) = palette(idx)
      val dr = r - (pr & 0xff)
      val dg = g - (pg & 0xff)
      val db = b - (pb & 0xff)
      val da = a - (pa & 0xff)
      dr * dr + dg * dg + db * db + da * da
    }
  }
}

/** Floyd-Steinberg error diffusion. Error weights (dx, dy, weight): right, down-left, down, down-right. */
object FloydSteinbergDithering extends ColorDithering {
  override def name: String = "Floyd-Steinberg"

  private val weights: List[(Int, Int, Double)] = List(
    (1, 0, 7.0 / 16),
    (-1, 1, 3.0 / 16),
    (0, 1, 5.0 / 16),
    (1, 1, 1.0 / 16)
  )

  override def quantizeToIndices(image: RawImage, palette: Vector[(Byte, Byte, Byte, Byte)]): Array[Int] = {
    val w   = image.width
    val h   = image.height
    val buf = Array.ofDim[Double](w * h * 4)
    for (i <- buf.indices) {
      buf(i) = image.data(i) & 0xff
    }
    val out = new Array[Int](w * h)
    for (y <- 0 until h; x <- 0 until w) {
      val idx = y * w + x
      val o   = idx * 4
      val r   = buf(o).toInt.max(0).min(255)
      val g   = buf(o + 1).toInt.max(0).min(255)
      val b   = buf(o + 2).toInt.max(0).min(255)
      val a   = buf(o + 3).toInt.max(0).min(255)
      val palIdx = NoColorDithering.nearestPaletteIndex(r, g, b, a, palette)
      out(idx) = palIdx
      val (pr, pg, pb, pa) = palette(palIdx)
      val qr = pr & 0xff
      val qg = pg & 0xff
      val qb = pb & 0xff
      val qa = pa & 0xff
      val er = r - qr
      val eg = g - qg
      val eb = b - qb
      val ea = a - qa
      for ((dx, dy, weight) <- weights) {
        val nx = x + dx
        val ny = y + dy
        if (nx >= 0 && nx < w && ny >= 0 && ny < h) {
          val nidx = ny * w + nx
          val no   = nidx * 4
          buf(no) += er * weight
          buf(no + 1) += eg * weight
          buf(no + 2) += eb * weight
          buf(no + 3) += ea * weight
        }
      }
    }
    out
  }
}

/** Ordered dithering using a Bayer matrix (Game Boy Camera style). Threshold is added before picking nearest palette color. */
final case class OrderedBayerDithering(matrixSize: Int) extends ColorDithering {
  override def name: String = s"Ordered (${matrixSize}×${matrixSize})"

  private val matrix: Array[Array[Double]] = bayerMatrix(matrixSize)
  private val scale = 32.0

  override def quantizeToIndices(image: RawImage, palette: Vector[(Byte, Byte, Byte, Byte)]): Array[Int] = {
    val w   = image.width
    val h   = image.height
    val n   = w * h
    val out = new Array[Int](n)
    for (i <- 0 until n) {
      val x = i % w
      val y = i / w
      val o = i * 4
      val thresh = (matrix(y % matrixSize)(x % matrixSize) * scale).toInt
      val r = ((image.data(o) & 0xff) + thresh).max(0).min(255)
      val g = ((image.data(o + 1) & 0xff) + thresh).max(0).min(255)
      val b = ((image.data(o + 2) & 0xff) + thresh).max(0).min(255)
      val a = (image.data(o + 3) & 0xff) & 0xff
      out(i) = NoColorDithering.nearestPaletteIndex(r, g, b, a, palette)
    }
    out
  }

  private def bayerMatrix(size: Int): Array[Array[Double]] = {
    def recurse(n: Int): Array[Array[Double]] =
      if (n <= 1) Array(Array(0.5))
      else {
        val half = recurse(n / 2)
        val scale = 1.0 / (n * n)
        Array.tabulate(n, n) { (r, c) =>
          val v = half(r % (n / 2))(c % (n / 2))
          val add = (r / (n / 2)) * 2 + (c / (n / 2))
          ((v * 4 + add) * scale) - 0.5
        }
      }
    recurse(size)
  }
}

object OrderedBayerDithering {
  val Size2: OrderedBayerDithering = OrderedBayerDithering(2)
  val Size4: OrderedBayerDithering = OrderedBayerDithering(4)
  val Size8: OrderedBayerDithering = OrderedBayerDithering(8)
}
