package clemniem.common.image

/** DOM-free RGBA image: width × height × 4 bytes (R,G,B,A per pixel, row-major). */
final case class RawImage(width: Int, height: Int, data: Array[Byte]) {
  require(width > 0 && height > 0 && data.length == width * height * 4, "Invalid RawImage dimensions or data length")

  def pixelCount: Int = width * height

  def getPixel(x: Int, y: Int): (Byte, Byte, Byte, Byte) = {
    val i = (y * width + x) * 4
    (data(i), data(i + 1), data(i + 2), data(i + 3))
  }

  def setPixel(x: Int, y: Int, r: Byte, g: Byte, b: Byte, a: Byte): Unit = {
    val i = (y * width + x) * 4
    data(i) = r
    data(i + 1) = g
    data(i + 2) = b
    data(i + 3) = a
  }

  def copy: RawImage = RawImage(width, height, data.clone())
}

object RawImage {
  def create(width: Int, height: Int): RawImage = {
    val data = new Array[Byte](width * height * 4)
    RawImage(width, height, data)
  }
}
