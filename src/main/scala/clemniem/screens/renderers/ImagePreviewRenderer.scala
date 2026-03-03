package clemniem.screens.renderers

import clemniem.PixelPic
import clemniem.common.CanvasUtils
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Pure canvas drawing for image previews (gallery thumbnails and upload preview). */
object ImagePreviewRenderer {

  /** Draw a scaled-to-fit gallery preview of a PixelPic. */
  def drawGalleryPreview(ctx: CanvasRenderingContext2D, pic: PixelPic, previewW: Int, previewH: Int): Unit = {
    ctx.clearRect(0, 0, previewW, previewH)
    if (pic.width > 0 && pic.height > 0) {
      val fit = CanvasUtils.scaleToFit(pic.width, pic.height, previewW, previewH, Double.MaxValue)
      CanvasUtils.drawPixelPic(ctx, pic, fit.width, fit.height, fit.offsetX, fit.offsetY)
    }
  }

  /** Draw a 1:1 preview: resize canvas to pic dimensions and render at native size. */
  def drawFullPreview(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic): Unit = {
    canvas.width = pic.width
    canvas.height = pic.height
    ctx.clearRect(0, 0, pic.width, pic.height)
    CanvasUtils.drawPixelPic(ctx, pic, pic.width, pic.height, 0, 0)
  }
}
