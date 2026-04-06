package clemniem.screens.renderers

import clemniem.Layout
import clemniem.common.CanvasUtils
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Pure canvas drawing for layout previews (editor grid and gallery thumbnails). */
object LayoutRenderer {

  /** Draw the editor grid: resize canvas to grid dimensions, checkerboard fills, thin borders. */
  def drawEditorGrid(canvas: Canvas, ctx: CanvasRenderingContext2D, grid: Layout): Unit = {
    canvas.width = grid.width
    canvas.height = grid.height
    ctx.clearRect(0, 0, grid.width, grid.height)
    ctx.lineWidth = 1
    grid.parts.zipWithIndex.foreach { case (part, i) =>
      ctx.fillStyle = if (i % 2 == 0) "#f5f5f5" else "#eee"
      ctx.fillRect(part.x, part.y, part.width, part.height)
      ctx.strokeStyle = "#333"
      ctx.strokeRect(part.x, part.y, part.width, part.height)
    }
  }

  /** NES.css-style gallery preview: thick dark borders, checkerboard fills, pixel-art bevel. */
  def drawGalleryPreview(ctx: CanvasRenderingContext2D, grid: Layout, previewW: Int, previewH: Int): Unit = {
    ctx.imageSmoothingEnabled = false
    ctx.clearRect(0, 0, previewW, previewH)
    if (grid.parts.nonEmpty && grid.width > 0 && grid.height > 0) {
      val margin = 4
      val fit    =
        CanvasUtils.scaleToFit(grid.width, grid.height, previewW - margin * 2, previewH - margin * 2, Double.MaxValue)
      val ox = fit.offsetX + margin
      val oy = fit.offsetY + margin

      grid.parts.zipWithIndex.foreach { case (part, i) =>
        val cx = ox + (part.x * fit.scale).toInt
        val cy = oy + (part.y * fit.scale).toInt
        val cw = (part.width * fit.scale).toInt.max(1)
        val ch = (part.height * fit.scale).toInt.max(1)
        ctx.fillStyle = if (i % 2 == 0) "#d4d4d8" else "#e4e4e7"
        ctx.fillRect(cx, cy, cw, ch)
        ctx.fillStyle = "rgba(0,0,0,0.18)"
        ctx.fillRect(cx + cw - 1, cy, 1, ch)
        ctx.fillRect(cx, cy + ch - 1, cw, 1)
        ctx.fillStyle = "rgba(255,255,255,0.45)"
        ctx.fillRect(cx, cy, cw, 1)
        ctx.fillRect(cx, cy, 1, ch)
      }

      val borderW = (2.0 / fit.scale).max(1.5).min(3.0)
      ctx.strokeStyle = "#212529"
      ctx.lineWidth = borderW * fit.scale
      grid.parts.foreach { part =>
        val cx = ox + (part.x * fit.scale).toInt
        val cy = oy + (part.y * fit.scale).toInt
        val cw = (part.width * fit.scale).toInt.max(1)
        val ch = (part.height * fit.scale).toInt.max(1)
        ctx.strokeRect(cx, cy, cw, ch)
      }

      val totalW = (grid.width * fit.scale).toInt.max(1)
      val totalH = (grid.height * fit.scale).toInt.max(1)
      ctx.lineWidth = (borderW * fit.scale * 1.25).min(4.0)
      ctx.strokeStyle = "#212529"
      ctx.strokeRect(ox, oy, totalW, totalH)
    }
  }
}
