package clemniem.common

import clemniem.{Color, Layout, PixelPic}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Canvas drawing helpers that depend on domain types ([[PixelPic]], [[Layout]], [[Color]]). Extracted from
  * [[CanvasUtils]] so the framework module has no domain-type imports.
  */
object PixelPicCanvas {

  /** Draw a PixelPic onto the canvas, scaled to targetWidth*targetHeight. Uses an offscreen buffer and
    * imageSmoothingEnabled = false for crisp pixel art. If pic is empty, clears the canvas.
    * @param dx
    *   x offset (e.g. for centering: (canvasWidth - targetWidth) / 2)
    * @param dy
    *   y offset (e.g. for centering: (canvasHeight - targetHeight) / 2)
    */
  def drawPixelPic(
    ctx: CanvasRenderingContext2D,
    pic: PixelPic,
    targetWidth: Int,
    targetHeight: Int,
    dx: Int,
    dy: Int
  ): Unit =
    if (targetWidth <= 0 || targetHeight <= 0) ()
    else if (pic.width <= 0 || pic.height <= 0) {
      ctx.clearRect(0, 0, targetWidth, targetHeight)
    } else {
      val (tmp, tctx) = ImageUtils.createOffscreenCanvas(pic.width, pic.height)
      val imgData     = tctx.createImageData(pic.width, pic.height)
      pic.fillImageData(imgData)
      tctx.putImageData(imgData, 0, 0)
      ctx.imageSmoothingEnabled = false
      ctx.drawImage(tmp, 0, 0, pic.width, pic.height, dx, dy, targetWidth, targetHeight)
    }

  /** Scale a full image to fit maxDim*maxDim, draw it, then overlay grid part strokes offset by (offsetX, offsetY). */
  def drawFullImageWithGrid(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    pic: PixelPic,
    grid: Layout,
    offsetX: Int,
    offsetY: Int,
    maxDim: Int
  ): Unit = {
    val fit = CanvasUtils.scaleToFit(pic.width, pic.height, maxDim, maxDim, 1.0)
    canvas.width = fit.width
    canvas.height = fit.height
    ctx.clearRect(0, 0, fit.width, fit.height)
    drawPixelPic(ctx, pic, fit.width, fit.height, 0, 0)
    ctx.strokeStyle = Color.errorStroke.rgba(0.8)
    ctx.lineWidth = 1
    val ox = (offsetX * fit.scale).toInt
    val oy = (offsetY * fit.scale).toInt
    grid.parts.foreach { part =>
      ctx.strokeRect(
        ox + part.x * fit.scale,
        oy + part.y * fit.scale,
        (part.width * fit.scale).max(1),
        (part.height * fit.scale).max(1))
    }
  }
}
