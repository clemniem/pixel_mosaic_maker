package clemniem.screens.renderers

import clemniem.{Color, Layout, PixelPic}
import clemniem.common.{CanvasUtils, PixelPicCanvas}
import org.scalajs.dom.CanvasRenderingContext2D

/** Pure canvas drawing for build gallery previews (cropped image + grid + step highlight). */
object BuildPreviewRenderer {

  /** Gallery card: cropped image at offset with grid overlay and optional current-step highlight. */
  def drawGalleryPreview(
    ctx: CanvasRenderingContext2D,
    picOpt: Option[PixelPic],
    grid: Layout,
    offsetX: Int,
    offsetY: Int,
    currentStep: Option[(Int, Int)],
    patchSize: Int,
    previewW: Int,
    previewH: Int
  ): Unit =
    picOpt match {
      case Some(pic) =>
        val gw  = grid.width
        val gh  = grid.height
        val fit = CanvasUtils.scaleToFit(gw, gh, previewW, previewH, 1.0)
        ctx.clearRect(0, 0, previewW, previewH)
        pic.crop(offsetX, offsetY, gw, gh) match {
          case Some(cropped) =>
            PixelPicCanvas.drawPixelPic(ctx, cropped, fit.width, fit.height, fit.offsetX, fit.offsetY)
            ctx.strokeStyle = Color.errorStroke.rgba(0.6)
            ctx.lineWidth = 1
            grid.parts.foreach { part =>
              ctx.strokeRect(
                fit.offsetX + part.x * fit.scale,
                fit.offsetY + part.y * fit.scale,
                (part.width * fit.scale).max(1),
                (part.height * fit.scale).max(1))
            }
            currentStep.foreach { case (sx, sy) =>
              val rx = sx - offsetX
              val ry = sy - offsetY
              ctx.strokeStyle = Color.highlightStroke.rgba(0.9)
              ctx.lineWidth = 2
              ctx.strokeRect(
                fit.offsetX + rx * fit.scale,
                fit.offsetY + ry * fit.scale,
                (patchSize * fit.scale).max(1),
                (patchSize * fit.scale).max(1))
            }
          case None =>
            CanvasUtils.drawCenteredErrorText(ctx, previewW, previewH, "Grid out of bounds")
        }
      case None =>
        CanvasUtils.drawCenteredErrorText(ctx, previewW, previewH, "Missing image/palette")
    }
}
