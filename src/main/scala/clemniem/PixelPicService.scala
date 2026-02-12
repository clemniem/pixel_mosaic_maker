package clemniem

import cats.effect.IO
import clemniem.common.ImageUtils.{
  DataUrlBase64,
  downscaleImageData,
  getFromImage,
  imageToImageData,
  imageToImageDataMaxSize,
  imageDataFromRaw,
  loadImageFromFile,
  rawFromImageData,
  detectNearestNeighborScale
}
import clemniem.common.image.{
  ColorDithering,
  ColorQuantizationService,
  DownscaleStrategy,
  SizeReductionService
}
import org.scalajs.dom.File

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

  /** Full pipeline: load from data URL, validate size, downscale to target max, optionally quantize. Returns Left(error) or Right(pic). */
  def processUploadedImage(
      dataUrl: DataUrlBase64,
      fileName: String,
      downscaleStrategy: DownscaleStrategy,
      numPaletteColors: Option[Int],
      colorDithering: ColorDithering
  ): IO[Either[String, PixelPic]] =
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
        val name = fileName
        val result = numPaletteColors match {
          case Some(n) =>
            val q = ColorQuantizationService.quantize(reduced, n, colorDithering)
            PixelPic.fromQuantized(reduced.width, reduced.height, q, name)
          case None =>
            val imgData2 = imageDataFromRaw(reduced)
            PixelPic.extractPixelImageFromImageData(imgData2).map(_.withName(name))
        }
        result.toRight("Could not process image")
      }
    }.attempt.map {
      case Right(Right(pic)) => Right(pic)
      case Right(Left(msg))  => Left(msg)
      case Left(e)           => Left(e.getMessage)
    }

  def fromDataUrl(dataUrl: DataUrlBase64): IO[Option[PixelPic]] =
    getFromImage(dataUrl)(img => PixelPic.extractPixelImageFromImage(img).map(_.withName("fromUrl")))
}
