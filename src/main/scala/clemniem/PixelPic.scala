package clemniem

import clemniem.common.ImageUtils.{createOffscreenCanvas, imageToImageData, DataUrlBase64}
import clemniem.common.image.QuantizedResult
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.scalajs.dom
import org.scalajs.dom.html.Image

import scala.collection.mutable

/** Pixel image: width, height, palette (lookup + counts), flat pixel indices. Storable in LocalStorage. */
final case class PixelPic private (
  width: Int,
  height: Int,
  paletteLookup: Vector[Pixel],
  pixels: Vector[Int],
  palette: Map[Int, Int],
  name: String) {

  lazy val pixelPalette: Map[Pixel, Int] = palette.map((k, v) => paletteLookup(k) -> v)

  lazy val size: ImgSize = ImgSize(width, height)

  def getPixel(x: Int, y: Int): Pixel =
    paletteLookup(pixels(y * width + x))

  def getIndexByCount: Vector[Int] =
    palette.toVector.sortBy(_._2).map(_._1)

  /** Fill the given ImageData buffer with RGBA pixel data from this PixelPic. The ImageData must have dimensions
    * matching this pic's width × height.
    */
  def fillImageData(imgData: dom.ImageData): Unit = {
    val data = imgData.data
    for (i <- pixels.indices) {
      val px     = paletteLookup(pixels(i))
      val offset = i * 4
      data(offset) = px.r
      data(offset + 1) = px.g
      data(offset + 2) = px.b
      data(offset + 3) = px.a
    }
  }

  def toImageData(mimeType: String): DataUrlBase64 = {
    val (canvas, ctx) = createOffscreenCanvas(width, height)
    val imgData       = ctx.createImageData(width, height)
    fillImageData(imgData)
    ctx.putImageData(imgData, 0, 0)
    canvas.toDataURL(mimeType)
  }

  def crop(x: Int, y: Int, w: Int, h: Int): Option[PixelPic] =
    if (x < 0 || y < 0 || x + w > width || y + h > height) None
    else {
      val counts = mutable.Map.empty[Int, Int]
      val croppedPixels = (for {
        cy <- 0 until h
        cx <- 0 until w
        p = pixels((y + cy) * width + (x + cx))
        _ = counts(p) = counts.getOrElse(p, 0) + 1
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

  def crop(rect: Rectangle): Option[PixelPic] =
    crop(rect.x, rect.y, rect.width, rect.height)

  def withName(newName: String): PixelPic = copy(name = newName)

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
  ): Option[PixelPic] =
    if (width * height != pixels.length) None
    else if (paletteLookup.isEmpty) None
    else if (!pixels.forall(i => i >= 0 && i < paletteLookup.length)) None
    else Some(new PixelPic(width, height, paletteLookup, pixels, pixelCounts, name))

  /** Build PixelPic from result of color quantization (e.g. from ColorQuantizationService). */
  def fromQuantized(
    width: Int,
    height: Int,
    result: QuantizedResult,
    name: String
  ): Option[PixelPic] =
    if (width * height != result.indices.length) None
    else if (result.palette.isEmpty) None
    else {
      val paletteLookup = result.palette.map { case (r, g, b, a) =>
        Pixel(r & 0xff, g & 0xff, b & 0xff, a & 0xff)
      }.toVector
      val counts = result.indices.groupBy(identity).view.mapValues(_.length).toMap
      PixelPic(
        width = width,
        height = height,
        paletteLookup = paletteLookup,
        pixels = result.indices.toVector,
        pixelCounts = counts,
        name = name
      ).map(sortPixelVector)
    }

  private[clemniem] def extractPixelImageFromImage(img: Image): Option[PixelPic] = {
    val imgData = imageToImageData(img)
    extractPixelImageFromImageData(imgData)
  }

  private[clemniem] def extractPixelImageFromImageData(imgData: org.scalajs.dom.ImageData): Option[PixelPic] = {
    val w    = imgData.width
    val h    = imgData.height
    val data = imgData.data

    val paletteBuf  = mutable.ArrayBuffer.empty[Pixel]
    val indexMap    = mutable.Map.empty[String, Int]
    val colorCounts = mutable.Map.empty[Int, Int]
    val pixels      = new Array[Int](w * h)

    for (i <- 0 until data.length by 4) {
      val r          = data(i).toInt & 0xff
      val g          = data(i + 1).toInt & 0xff
      val b          = data(i + 2).toInt & 0xff
      val a          = data(i + 3).toInt & 0xff
      val px         = Pixel(r, g, b, a)
      val colorValue = s"$r,$g,$b,$a"
      val index = indexMap.getOrElseUpdate(
        colorValue, {
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

  private[clemniem] def sortPixelVector(pp: PixelPic): PixelPic = {
    val sortedPixels  = pp.paletteLookup.sortBy(_.brightness)
    val pixelToNewIdx = sortedPixels.zipWithIndex.toMap
    val oldIndexToNew = (0 until pp.paletteLookup.size).iterator
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
