package clemniem.screens.renderers

import clemniem.{Color, Layout, PixelPic}
import clemniem.common.CanvasUtils
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Pure canvas drawing for build-config previews (gallery thumbnails and editor canvases). */
object BuildConfigRenderer {

  /** Gallery card: cropped image at offset with grid overlay, or error text if missing. */
  def drawGalleryPreview(
    ctx: CanvasRenderingContext2D,
    picOpt: Option[PixelPic],
    grid: Layout,
    offsetX: Int,
    offsetY: Int,
    previewW: Int,
    previewH: Int
  ): Unit =
    picOpt match {
      case Some(pic) =>
        pic.crop(offsetX, offsetY, grid.width, grid.height) match {
          case Some(cropped) =>
            val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, previewW, previewH, 1.0)
            ctx.clearRect(0, 0, previewW, previewH)
            CanvasUtils.drawPixelPic(ctx, cropped, fit.width, fit.height, fit.offsetX, fit.offsetY)
            ctx.strokeStyle = Color.errorStroke.rgba(0.7)
            ctx.lineWidth = 1
            grid.parts.foreach { part =>
              ctx.strokeRect(
                fit.offsetX + part.x * fit.scale,
                fit.offsetY + part.y * fit.scale,
                (part.width * fit.scale).max(1),
                (part.height * fit.scale).max(1))
            }
          case None =>
            CanvasUtils.drawCenteredErrorText(ctx, previewW, previewH, "Grid out of bounds")
        }
      case None =>
        CanvasUtils.drawCenteredErrorText(ctx, previewW, previewH, "Missing image/palette")
    }

  /** Editor preview: cropped region with grid overlay, scaled to fit maxDim. */
  def drawCroppedRegion(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    pic: PixelPic,
    grid: Layout,
    offsetX: Int,
    offsetY: Int,
    maxDim: Int
  ): Unit =
    pic.crop(offsetX, offsetY, grid.width, grid.height) match {
      case Some(cropped) =>
        val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, maxDim, maxDim, 1.0)
        canvas.width = fit.width
        canvas.height = fit.height
        ctx.clearRect(0, 0, fit.width, fit.height)
        CanvasUtils.drawPixelPic(ctx, cropped, fit.width, fit.height, 0, 0)
        ctx.strokeStyle = Color.errorStroke.rgba(0.8)
        ctx.lineWidth = 1
        grid.parts.foreach { part =>
          ctx.strokeRect(
            part.x * fit.scale,
            part.y * fit.scale,
            (part.width * fit.scale).max(1),
            (part.height * fit.scale).max(1))
        }
      case None =>
        CanvasUtils.drawPlaceholder(canvas, ctx, maxDim, maxDim / 2, "Layout area is outside the image")
    }

  /** Editor overview: full image scaled to fit maxDim, no grid. */
  def drawFullImage(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic, maxDim: Int): Unit = {
    val fit = CanvasUtils.scaleToFit(pic.width, pic.height, maxDim, maxDim, 1.0)
    canvas.width = fit.width
    canvas.height = fit.height
    ctx.clearRect(0, 0, fit.width, fit.height)
    CanvasUtils.drawPixelPic(ctx, pic, fit.width, fit.height, 0, 0)
  }
}
