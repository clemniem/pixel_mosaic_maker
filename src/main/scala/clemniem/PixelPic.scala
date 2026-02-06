package clemniem

import cats.effect.IO
import clemniem.common.ImageUtils.{
  DataUrlBase64,
  downscaleImageData,
  getFromImage,
  imageToImageData,
  loadImageFromFile,
  detectNearestNeighborScale
}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.scalajs.dom
import org.scalajs.dom.File
import org.scalajs.dom.html.{Canvas, Image}

import scala.collection.mutable

/** Pixel image: width, height, palette (lookup + counts), flat pixel indices. Storable in LocalStorage. */
final case class PixelPic private (
    width: Int,
    height: Int,
    paletteLookup: Vector[Pixel],
    pixels: Vector[Int],
    palette: Map[Int, Int],
    name: String
) {

  def dataUrl: DataUrlBase64 = toImageData("image/png")

  lazy val pixelPalette: Map[Pixel, Int] = palette.map((k, v) => paletteLookup(k) -> v)

  lazy val size: ImgSize = ImgSize(width, height)

  def getPixel(x: Int, y: Int): Pixel =
    paletteLookup(pixels(y * width + x))

  def getIndexByCount: Vector[Int] =
    palette.toVector.sortBy(_._2).map(_._1)

  def toImageData(mimeType: String): DataUrlBase64 = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = width
    canvas.height = height
    val ctx     = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imgData = ctx.createImageData(width, height)
    val data    = imgData.data
    for (i <- pixels.indices) {
      val pixel  = paletteLookup(pixels(i))
      val offset = i * 4
      data(offset) = pixel.r.toByte
      data(offset + 1) = pixel.g.toByte
      data(offset + 2) = pixel.b.toByte
      data(offset + 3) = pixel.a.toByte
    }
    ctx.putImageData(imgData, 0, 0)
    canvas.toDataURL(mimeType)
  }

  def crop(x: Int, y: Int, w: Int, h: Int): Option[PixelPic] = {
    if (x < 0 || y < 0 || x + w > width || y + h > height) None
    else {
      val counts = mutable.Map.empty[Int, Int]
      val croppedPixels = (for {
        cy <- 0 until h
        cx <- 0 until w
        p  = pixels((y + cy) * width + (x + cx))
        _  = counts(p) = counts.getOrElse(p, 0) + 1
      } yield p).toVector
      PixelPic(
        width = w,
        height = h,
        paletteLookup = paletteLookup,
        pixels = croppedPixels,
        pixelCounts = counts.toMap,
        name = name + s"_crop_${x}_${y}_${w}_${h}"
      )
    }
  }

  def crop(rect: Rectangle): Option[PixelPic] =
    crop(rect.x, rect.y, rect.width, rect.height)

  def setPalette(newPalette: Vector[Pixel]): PixelPic =
    copy(paletteLookup = newPalette.take(paletteLookup.size))
}

object PixelPic {

  import clemniem.common.circe.MapAsList.given

  given Encoder[PixelPic] = deriveEncoder
  given Decoder[PixelPic] = deriveDecoder

  def apply(
      width: Int,
      height: Int,
      paletteLookup: Vector[Pixel],
      pixels: Vector[Int],
      pixelCounts: Map[Int, Int],
      name: String
  ): Option[PixelPic] = {
    if (width * height != pixels.length) None
    else if (paletteLookup.isEmpty) None
    else if (!pixels.forall(i => i >= 0 && i < paletteLookup.length)) None
    else Some(new PixelPic(width, height, paletteLookup, pixels, pixelCounts, name))
  }

  /** Load from file: detect nearest-neighbor scale, downscale to pixel size, then extract. */
  def loadPixelImageFromFile(file: File): IO[Option[PixelPic]] =
    for {
      (fileName, dataUrl) <- loadImageFromFile(file)
      pic                 <- getFromImage(dataUrl) { img =>
        val imgData0 = imageToImageData(img)
        val scaleOpt = detectNearestNeighborScale(imgData0)
        val imgData  = scaleOpt.fold(imgData0)(f => downscaleImageData(imgData0, f))
        extractPixelImageFromImageData(imgData).map(_.copy(name = fileName))
      }
    } yield pic

  def fromDataUrl(dataUrl: DataUrlBase64): IO[Option[PixelPic]] =
    getFromImage(dataUrl)(img => extractPixelImageFromImage(img).map(_.copy(name = "fromUrl")))

  private def extractPixelImageFromImage(img: Image): Option[PixelPic] = {
    val imgData = imageToImageData(img)
    extractPixelImageFromImageData(imgData)
  }

  private def extractPixelImageFromImageData(imgData: org.scalajs.dom.ImageData): Option[PixelPic] = {
    val w    = imgData.width
    val h    = imgData.height
    val data = imgData.data

    val paletteBuf   = mutable.ArrayBuffer.empty[Pixel]
    val indexMap     = mutable.Map.empty[String, Int]
    val colorCounts  = mutable.Map.empty[Int, Int]
    val pixels       = new Array[Int](w * h)

    for (i <- 0 until data.length by 4) {
      val r = data(i).toInt & 0xff
      val g = data(i + 1).toInt & 0xff
      val b = data(i + 2).toInt & 0xff
      val a = data(i + 3).toInt & 0xff
      val px         = Pixel(r, g, b, a)
      val colorValue = s"$r,$g,$b,$a"
      val index = indexMap.getOrElseUpdate(colorValue, {
        val newIndex = paletteBuf.size
        paletteBuf += px
        newIndex
      })
      colorCounts(index) = colorCounts.getOrElse(index, 0) + 1
      pixels(i / 4) = index
    }

    PixelPic(
      width = w,
      height = h,
      paletteLookup = paletteBuf.toVector,
      pixels = pixels.toVector,
      pixelCounts = colorCounts.toMap,
      name = "extracted"
    ).map(pp => sortPixelVector(pp))
  }

  private def sortPixelVector(pp: PixelPic): PixelPic = {
    val sortedPixels   = pp.paletteLookup.sortBy(_.brightness)
    val pixelToNewIdx  = sortedPixels.zipWithIndex.toMap
    val oldIndexToNew  = (0 until pp.paletteLookup.size).iterator
      .map(i => i -> pixelToNewIdx(pp.paletteLookup(i)))
      .toMap
    if (oldIndexToNew.isEmpty) pp
    else
      pp.copy(
        paletteLookup = sortedPixels,
        pixels = pp.pixels.map(idx => oldIndexToNew.getOrElse(idx, idx))
      )
  }
}
