package clemniem.screens.renderers

import clemniem.{Color, Layout, PixelPic}
import clemniem.common.CanvasUtils
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Pure canvas drawing for the build step screen (overview + per-step color patches). */
object BuildStepRenderer {

  /** Overview: cropped image with grid overlay and current-step highlight. */
  def drawOverview(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    pic: PixelPic,
    grid: Layout,
    offsetX: Int,
    offsetY: Int,
    currentStep: Option[(Int, Int)],
    patchSize: Int,
    maxDim: Int
  ): Unit =
    pic.crop(offsetX, offsetY, grid.width, grid.height) match {
      case Some(cropped) =>
        val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, maxDim, maxDim, 1.0)
        canvas.width = fit.width
        canvas.height = fit.height
        ctx.clearRect(0, 0, fit.width, fit.height)
        CanvasUtils.drawPixelPic(ctx, cropped, fit.width, fit.height, 0, 0)
        ctx.strokeStyle = Color.errorStroke.rgba(0.6)
        ctx.lineWidth = 1
        grid.parts.foreach { part =>
          ctx.strokeRect(
            part.x * fit.scale,
            part.y * fit.scale,
            (part.width * fit.scale).max(1),
            (part.height * fit.scale).max(1))
        }
        currentStep.foreach { case (sx, sy) =>
          val rx = sx - offsetX
          val ry = sy - offsetY
          ctx.strokeStyle = Color.highlightStroke.rgba(0.9)
          ctx.lineWidth = 2
          ctx.strokeRect(
            rx * fit.scale,
            ry * fit.scale,
            (patchSize * fit.scale).max(1),
            (patchSize * fit.scale).max(1))
        }
      case None =>
        CanvasUtils.drawPlaceholder(canvas, ctx, maxDim, maxDim / 2, "Grid region out of bounds")
    }

  /** Step preview: per-color patches (stacked or individual). Each color gets its own cell. */
  def drawStepPreview(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    patchOpt: Option[PixelPic],
    bgHex: String,
    stacked: Boolean,
    cols: Int,
    patchSize: Int
  ): Unit =
    patchOpt match {
      case Some(patch) =>
        val sortedColors    = colorsByCountAsc(patch)
        val cellPx          = 8
        val cellW           = patchSize * cellPx
        val cellH           = patchSize * cellPx
        val gap             = 8
        val gridStep        = 4
        val (bgR, bgG, bgB) = patchBackgroundRgb(bgHex)

        if (stacked) {
          drawStacked(canvas, ctx, patch, sortedColors, cellPx, cellW, cellH, gap, gridStep, cols, bgR, bgG, bgB, patchSize)
        } else {
          drawGrid(canvas, ctx, patch, sortedColors, cellPx, cellW, cellH, gap, gridStep, cols, bgR, bgG, bgB, patchSize)
        }
      case None =>
        canvas.width = patchSize
        canvas.height = patchSize
        val (r, g, b) = patchBackgroundRgb(bgHex)
        ctx.fillStyle = s"rgb($r,$g,$b)"
        ctx.fillRect(0, 0, patchSize, patchSize)
    }

  /** Colors in patch sorted by count ascending (least to most). */
  def colorsByCountAsc(patch: PixelPic): Vector[(Int, Int)] =
    patch.palette.toVector.sortBy(_._2)

  /** Parse hex color to RGB, falling back to (238,238,238). */
  def patchBackgroundRgb(hex: String): (Int, Int, Int) = {
    val h = Color.normalizeHex(hex, Color.layerPatchBackground.toHex)
    def parse(s: String): Int = {
      val n = java.lang.Integer.parseInt(s, 16)
      if (n >= 0 && n <= 255) n else 238
    }
    if (h.length != 7) (238, 238, 238)
    else
      try (parse(h.substring(1, 3)), parse(h.substring(3, 5)), parse(h.substring(5, 7)))
      catch { case _: Exception => (238, 238, 238) }
  }

  /** Stacked mode: each cell shows cumulative layers (layer 0 = color 0 only, layer 1 = colors 0+1, etc). */
  private def drawStacked(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    patch: PixelPic,
    sortedColors: Vector[(Int, Int)],
    cellPx: Int,
    cellW: Int,
    cellH: Int,
    gap: Int,
    gridStep: Int,
    cols: Int,
    bgR: Int,
    bgG: Int,
    bgB: Int,
    patchSize: Int
  ): Unit = {
    val n      = sortedColors.size.min(16)
    val rows   = if (n <= 0) 0 else (n + cols - 1) / cols
    val totalW = if (cols <= 0) 1 else cols * cellW + (cols - 1) * gap
    val totalH = if (rows <= 0) 1 else rows * cellH + (rows - 1) * gap
    canvas.width = totalW.max(1)
    canvas.height = totalH.max(1)
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    sortedColors.zipWithIndex.take(16).foreach { case ((_, _), layerIdx) =>
      val col      = layerIdx % cols
      val row      = layerIdx / cols
      val ox       = col * (cellW + gap)
      val oy       = row * (cellH + gap)
      val colorSet = sortedColors.take(layerIdx + 1).map(_._1).toSet
      val imgData  = ctx.createImageData(cellW, cellH)
      val data     = imgData.data
      for {
        y <- 0 until patchSize
        x <- 0 until patchSize
      } {
        val idx = y * patchSize + x
        val (r, g, b) =
          if (colorSet.contains(patch.pixels(idx))) {
            val px = patch.paletteLookup(patch.pixels(idx))
            (px.r, px.g, px.b)
          } else (bgR, bgG, bgB)
        for {
          py    <- 0 until cellPx
          pxOff <- 0 until cellPx
        } {
          val off = ((y * cellPx + py) * cellW + (x * cellPx + pxOff)) * 4
          data(off) = r
          data(off + 1) = g
          data(off + 2) = b
          data(off + 3) = 255
        }
      }
      ctx.putImageData(imgData, ox, oy)
      drawCellGridLines(ctx, ox, oy, cellW, cellH, gridStep, cellPx)
    }
  }

  /** Grid mode: each color in its own cell, showing only that color's pixels. */
  private def drawGrid(
    canvas: Canvas,
    ctx: CanvasRenderingContext2D,
    patch: PixelPic,
    sortedColors: Vector[(Int, Int)],
    cellPx: Int,
    cellW: Int,
    cellH: Int,
    gap: Int,
    gridStep: Int,
    cols: Int,
    bgR: Int,
    bgG: Int,
    bgB: Int,
    patchSize: Int
  ): Unit = {
    val n      = sortedColors.size.min(16)
    val rows   = if (n <= 0) 0 else (n + cols - 1) / cols
    val totalW = if (cols <= 0) 1 else cols * cellW + (cols - 1) * gap
    val totalH = if (rows <= 0) 1 else rows * cellH + (rows - 1) * gap
    canvas.width = totalW.max(1)
    canvas.height = totalH.max(1)
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    sortedColors.zipWithIndex.take(16).foreach { case ((paletteIndex, _), i) =>
      val px      = patch.paletteLookup(paletteIndex)
      val col     = i % cols
      val row     = i / cols
      val ox      = col * (cellW + gap)
      val oy      = row * (cellH + gap)
      val imgData = ctx.createImageData(cellW, cellH)
      val data    = imgData.data
      for {
        y <- 0 until patchSize
        x <- 0 until patchSize
      } {
        val idx = y * patchSize + x
        val (r, g, b) =
          if (patch.pixels(idx) == paletteIndex) (px.r, px.g, px.b)
          else (bgR, bgG, bgB)
        for {
          py    <- 0 until cellPx
          pxOff <- 0 until cellPx
        } {
          val off = ((y * cellPx + py) * cellW + (x * cellPx + pxOff)) * 4
          data(off) = r
          data(off + 1) = g
          data(off + 2) = b
          data(off + 3) = 255
        }
      }
      ctx.putImageData(imgData, ox, oy)
      drawCellGridLines(ctx, ox, oy, cellW, cellH, gridStep, cellPx)
    }
  }

  /** Draw quarter-grid lines on a single cell for visual alignment. */
  private def drawCellGridLines(
    ctx: CanvasRenderingContext2D,
    ox: Int,
    oy: Int,
    cellW: Int,
    cellH: Int,
    gridStep: Int,
    cellPx: Int
  ): Unit = {
    ctx.strokeStyle = Color.black.rgba(0.45)
    ctx.lineWidth = 1
    for (g <- 1 until 4) {
      val pos = g * gridStep * cellPx
      ctx.beginPath()
      ctx.moveTo(ox + pos, oy)
      ctx.lineTo(ox + pos, oy + cellH)
      ctx.stroke()
      ctx.beginPath()
      ctx.moveTo(ox, oy + pos)
      ctx.lineTo(ox + cellW, oy + pos)
      ctx.stroke()
    }
  }
}
