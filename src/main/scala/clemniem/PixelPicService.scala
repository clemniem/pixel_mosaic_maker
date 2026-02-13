package clemniem

import cats.effect.IO
import clemniem.common.ImageUtils.{
  DataUrlBase64,
  downscaleImageData,
  getFromImage,
  imageToImageData,
  imageToImageDataMaxSize,
  loadImageFromFile,
  rawFromImageData,
  detectNearestNeighborScale
}
import clemniem.common.image.{
  ColorDithering,
  ColorQuantizationService,
  DownscaleStrategy,
  QuantizedResult,
  RawImage,
  SizeReductionService
}
import org.scalajs.dom.File

/** Palette mode for the image upload pipeline. */
sealed trait PaletteMode
/** Auto-quantize using median cut to N colors (4–16). */
final case class AutoQuantize(numColors: Int) extends PaletteMode
/** Use a fixed palette directly (e.g. from a saved StoredPalette). */
final case class FromPalette(palette: Vector[(Byte, Byte, Byte, Byte)]) extends PaletteMode

/** IO-based image loading and processing that produces [[PixelPic]] instances.
  * Separated from the [[PixelPic]] data model to keep concerns distinct.
  */
object PixelPicService {

  /** Load from file: detect nearest-neighbor scale, downscale to pixel size, then extract. */
  def loadPixelImageFromFile(file: File): IO[Option[PixelPic]] =
    for {
      (fileName, dataUrl) <- loadImageFromFile(file)
      pic                 <- getFromImage(dataUrl) { img =>
        val imgData0 = imageToImageData(img)
        val scaleOpt = detectNearestNeighborScale(imgData0)
        val imgData  = scaleOpt.fold(imgData0)(f => downscaleImageData(imgData0, f))
        PixelPic.extractPixelImageFromImageData(imgData).map(_.withName(fileName))
      }
    } yield pic

  /** Full pipeline: load from data URL, validate size, downscale to target max, quantize.
    * Returns Left(error) or Right((pic, detectedUniqueColors)) where detectedUniqueColors is
    * the number of unique colors in the downscaled image before quantization.
    */
  def processUploadedImage(
      dataUrl: DataUrlBase64,
      fileName: String,
      downscaleStrategy: DownscaleStrategy,
      paletteMode: PaletteMode,
      colorDithering: ColorDithering
  ): IO[Either[String, (PixelPic, Int)]] =
    getFromImage(dataUrl) { img =>
      val w = img.width
      val h = img.height
      if (SizeReductionService.exceedsMaxUpload(w, h))
        Left(s"Image too large. Max ${SizeReductionService.MaxUploadWidth}×${SizeReductionService.MaxUploadHeight} px (got ${w}×${h}).")
      else {
        val imgData = imageToImageDataMaxSize(
          img,
          SizeReductionService.TargetMaxWidth,
          SizeReductionService.TargetMaxHeight
        )
        val raw    = rawFromImageData(imgData)
        val reduced = SizeReductionService.downscale(
          raw,
          SizeReductionService.TargetMaxWidth,
          SizeReductionService.TargetMaxHeight,
          downscaleStrategy
        )
        val uniqueColors = countUniqueColors(reduced)
        val name = fileName
        val result = paletteMode match {
          case AutoQuantize(n) =>
            val q = ColorQuantizationService.quantize(reduced, n, colorDithering)
            PixelPic.fromQuantized(reduced.width, reduced.height, q, name)
          case FromPalette(pal) =>
            val indices = colorDithering.quantizeToIndices(reduced, pal)
            PixelPic.fromQuantized(reduced.width, reduced.height, QuantizedResult(pal, indices), name)
        }
        result.map(pic => (pic, uniqueColors)).toRight("Could not process image")
      }
    }.attempt.map {
      case Right(Right(v))  => Right(v)
      case Right(Left(msg)) => Left(msg)
      case Left(e)          => Left(e.getMessage)
    }

  /** Count unique RGBA colors in a raw image. */
  private def countUniqueColors(raw: RawImage): Int = {
    val seen = scala.collection.mutable.HashSet.empty[Long]
    val n = raw.width * raw.height
    for (i <- 0 until n) {
      val o = i * 4
      val r = raw.data(o) & 0xff
      val g = raw.data(o + 1) & 0xff
      val b = raw.data(o + 2) & 0xff
      val a = raw.data(o + 3) & 0xff
      seen += ((r.toLong << 24) | (g.toLong << 16) | (b.toLong << 8) | a.toLong)
    }
    seen.size
  }

  def fromDataUrl(dataUrl: DataUrlBase64): IO[Option[PixelPic]] =
    getFromImage(dataUrl)(img => PixelPic.extractPixelImageFromImage(img).map(_.withName("fromUrl")))
}
