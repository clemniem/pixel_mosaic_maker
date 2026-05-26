package clemniem.common.pdf

import clemniem.{Color, Layout, Pixel, PixelPic}

/** Low-level drawing primitives, geometry helpers, and pixel conversion for PDF generation. */
private[common] object PdfDrawHelpers {

  val smallOverviewGreyOpacity         = 0.4
  val smallOverviewHighlightFrameMm    = 0.25
  val smallOverviewOuterBorderMm       = 0.35
  val explodedPartFrameLineWidthMm     = 0.25
  val smallOverviewStepGridLineWidthMm = 0.15
  val smallOverviewStepGridDashMm      = 0.9
  val smallOverviewStepGridGapMm       = 1.0

  /** Row-major flat RGB (3 ints per pixel) for use in DrawPixelGrid. */
  def pixelPicToRgbFlat(pic: PixelPic): (Int, Int, Vector[Int]) = {
    val flat = pic.pixels.iterator.flatMap { i =>
      val p = pic.paletteLookup(i)
      Vector(p.r, p.g, p.b)
    }.toVector
    (pic.width, pic.height, flat)
  }

  /** Full-resolution RGB flat: pixels in the color set get their color, rest get the given background. */
  def buildLayerRgb(
    sectionPic: PixelPic,
    colorIndexSet: Set[Int],
    paletteLookup: Int => Pixel,
    bgRgb: (Int, Int, Int)
  ): Vector[Int] =
    sectionPic.pixels.iterator.flatMap { idx =>
      val (r, g, b) =
        if (colorIndexSet.contains(idx)) {
          val p = paletteLookup(idx)
          (p.r, p.g, p.b)
        } else bgRgb
      Vector(r, g, b)
    }.toVector

  /** One pixel-count row: swatch (with black frame) + "× count". Uses [[Instruction.DrawSwatchRow]] (text centered with
    * swatch).
    */
  def drawSwatchRow(x: Double, y: Double, r: Int, g: Int, b: Int, count: Int, sw: PdfLayoutConfig.SwatchBlock)
    : List[Instruction] =
    List(
      Instruction.DrawSwatchRow(
        x,
        y,
        r,
        g,
        b,
        count,
        sw.swatchSizeMm,
        sw.swatchGapMm,
        sw.countFontSizePt,
        sw.swatchStrokeLineWidthMm))

  /** Multiple pixel-count rows. Each row uses [[drawSwatchRow]]; y advances by sw.lineHeightMm (includes padding
    * between rows).
    */
  def drawSwatchRows(x: Double, yStart: Double, rows: Seq[(Int, Int, Int, Int)], sw: PdfLayoutConfig.SwatchBlock)
    : List[Instruction] =
    rows.zipWithIndex.flatMap { case ((r, g, b, count), i) =>
      drawSwatchRow(x, yStart + sw.firstLineOffsetMm + i * sw.lineHeightMm, r, g, b, count, sw)
    }.toList

  /** Stroke rects for a 4×4 grid overlay: 3 vertical + 3 horizontal lines. */
  def grid4x4StrokeRects(x0: Double, y0: Double, imageW: Double, imageH: Double, lineWidthMm: Double)
    : List[(Double, Double, Double, Double)] = {
    val half        = lineWidthMm / 2
    val cellW       = imageW / 4
    val cellH       = imageH / 4
    val verticals   = (1 to 3).map(i => (x0 + i * cellW - half, y0, lineWidthMm, imageH)).toList
    val horizontals = (1 to 3).map(i => (x0, y0 + i * cellH - half, imageW, lineWidthMm)).toList
    verticals ++ horizontals
  }

  /** Stroke rects for 16×16 step grid: one line every patchSizePx pixels in section space. */
  def grid16x16PatchStrokeRects(
    x0: Double,
    y0: Double,
    imageW: Double,
    imageH: Double,
    sectionWidth: Int,
    sectionHeight: Int,
    patchSizePx: Int,
    lineWidthMm: Double
  ): List[(Double, Double, Double, Double)] = {
    val half      = lineWidthMm / 2
    val nCols     = sectionWidth / patchSizePx
    val nRows     = sectionHeight / patchSizePx
    val pxW       = imageW / sectionWidth
    val pxH       = imageH / sectionHeight
    val verticals = (1 until nCols).map { i =>
      val x = x0 + i * patchSizePx * pxW - half
      (x, y0, lineWidthMm, imageH)
    }.toList
    val horizontals = (1 until nRows).map { j =>
      val y = y0 + j * patchSizePx * pxH - half
      (x0, y, imageW, lineWidthMm)
    }.toList
    verticals ++ horizontals
  }

  /** Instructions to draw a line as white full line with dashed black on top (for step grid). (x1,y1) to (x2,y2),
    * lineWidthMm, dash and gap in mm. Includes a final partial dash so black runs to the end.
    */
  def dashedLineInstructions(
    x1: Double,
    y1: Double,
    x2: Double,
    y2: Double,
    lineWidthMm: Double,
    dashLengthMm: Double,
    gapMm: Double
  ): List[Instruction] = {
    val length = math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    if (length <= 0) Nil
    else {
      val white       = Instruction.DrawLine(x1, y1, x2, y2, lineWidthMm, Color.white.r, Color.white.g, Color.white.b)
      val period      = dashLengthMm + gapMm
      val nFullDashes = (length / period).floor.toInt.max(0)
      val fullBlackDashes = (0 until nFullDashes).flatMap { i =>
        val startMm = i * period
        val endMm   = (i * period + dashLengthMm).min(length)
        if (endMm <= startMm) Nil
        else {
          val startT = startMm / length
          val endT   = endMm / length
          val sx     = x1 + startT * (x2 - x1)
          val sy     = y1 + startT * (y2 - y1)
          val ex     = x1 + endT * (x2 - x1)
          val ey     = y1 + endT * (y2 - y1)
          List(Instruction.DrawLine(sx, sy, ex, ey, lineWidthMm, Color.black.r, Color.black.g, Color.black.b))
        }
      }.toList
      val remainderStart = nFullDashes * period
      val finalDash      =
        if (remainderStart < length && dashLengthMm > 0) {
          val startT = remainderStart / length
          val ex     = x1 + 1.0 * (x2 - x1)
          val ey     = y1 + 1.0 * (y2 - y1)
          val sx     = x1 + startT * (x2 - x1)
          val sy     = y1 + startT * (y2 - y1)
          List(Instruction.DrawLine(sx, sy, ex, ey, lineWidthMm, Color.black.r, Color.black.g, Color.black.b))
        } else Nil
      white :: (fullBlackDashes ++ finalDash)
    }
  }

  /** Convert stroke rects (thin lines as rects) to dashed white+black line instructions. Each rect (x,y,w,h): if w>=h
    * horizontal else vertical; line thickness from rect.
    */
  def dashedStrokeRectsInstructions(
    rects: List[(Double, Double, Double, Double)],
    dashLengthMm: Double,
    gapMm: Double
  ): List[Instruction] = {
    val dashLen = if (dashLengthMm > 0) dashLengthMm else 1.5
    val gap     = if (gapMm >= 0) gapMm else 1.0
    rects.flatMap { case (x, y, w, h) =>
      val (x1, y1, x2, y2) =
        if (w >= h) {
          val cy = y + h / 2
          (x, cy, x + w, cy)
        } else {
          val cx = x + w / 2
          (cx, y, cx, y + h)
        }
      val lw = if (w >= h) h else w
      dashedLineInstructions(x1, y1, x2, y2, lw, dashLen, gap)
    }
  }

  /** Layout params for the small overview: (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH). */
  def smallOverviewLayoutParams(
    fullPic: PixelPic,
    gridOverviewY: Double,
    marginLR: Double,
    co: PdfLayoutConfig.ChapterOverview
  ): (Double, Double, Double, Double, Double) = {
    val smallAreaW     = co.colorListReservedWidthMm - co.gridOverviewLeftMarginMm - co.gridOverviewRightMarginMm
    val smallAreaH     = co.gridOverviewMaxHeightMm
    val smallScale     = (smallAreaW / fullPic.width).min(smallAreaH / fullPic.height)
    val smallOverviewW = fullPic.width * smallScale
    val smallOverviewH = fullPic.height * smallScale
    (marginLR, gridOverviewY, smallScale, smallOverviewW, smallOverviewH)
  }

  /** Stroke rects for layout grid over full mosaic (small overview): lines at grid part boundaries, not step
    * subdivisions.
    */
  def smallOverviewLayoutGridStrokeRects(
    x0: Double,
    y0: Double,
    imageW: Double,
    imageH: Double,
    grid: Layout,
    mosaicWidthPx: Int,
    mosaicHeightPx: Int,
    lineWidthMm: Double
  ): List[(Double, Double, Double, Double)] = {
    val half      = lineWidthMm / 2
    val parts     = grid.parts.toList
    val scaleX    = imageW / mosaicWidthPx
    val scaleY    = imageH / mosaicHeightPx
    val allX      = parts.flatMap(p => List(p.x, p.x + p.width)).distinct.sorted
    val allY      = parts.flatMap(p => List(p.y, p.y + p.height)).distinct.sorted
    val interiorX = allX.drop(1).dropRight(1)
    val interiorY = allY.drop(1).dropRight(1)
    val verticals = interiorX.map { xPx =>
      val x = x0 + xPx * scaleX - half
      (x, y0, lineWidthMm, imageH)
    }
    val horizontals = interiorY.map { yPx =>
      val y = y0 + yPx * scaleY - half
      (x0, y, imageW, lineWidthMm)
    }
    verticals ++ horizontals
  }

  /** Draw the small overview: full mosaic, grey overlays on greyRectsMm (everything except highlighted), dashed layout
    * grid (part boundaries), black frame on highlightedRectMm, then outer border.
    */
  def drawSmallOverview(
    fullPic: PixelPic,
    smallX0: Double,
    smallY0: Double,
    smallOverviewW: Double,
    smallOverviewH: Double,
    greyRectsMm: List[(Double, Double, Double, Double)],
    highlightedRectMm: (Double, Double, Double, Double),
    grid: Layout
  ): List[Instruction] = {
    val (_, _, fullRgbFlat) = pixelPicToRgbFlat(fullPic)
    val gridInstr           = List(
      Instruction
        .DrawPixelGrid(smallX0, smallY0, smallOverviewW, smallOverviewH, fullPic.width, fullPic.height, fullRgbFlat))
    val greyInstrs = greyRectsMm.map { case (x, y, w, h) =>
      Instruction.FillRectWithOpacity(
        x,
        y,
        w,
        h,
        Color.smallOverviewGrey.r,
        Color.smallOverviewGrey.g,
        Color.smallOverviewGrey.b,
        smallOverviewGreyOpacity)
    }
    val layoutGridRects = smallOverviewLayoutGridStrokeRects(
      smallX0,
      smallY0,
      smallOverviewW,
      smallOverviewH,
      grid,
      fullPic.width,
      fullPic.height,
      smallOverviewStepGridLineWidthMm)
    val layoutGridDashed =
      dashedStrokeRectsInstructions(layoutGridRects, smallOverviewStepGridDashMm, smallOverviewStepGridGapMm)
    val frameInstr = List(
      Instruction.DrawStrokeRects(
        List(highlightedRectMm),
        Color.black.r,
        Color.black.g,
        Color.black.b,
        smallOverviewHighlightFrameMm))
    val borderInstr = List(
      Instruction.DrawStrokeRects(
        List((smallX0, smallY0, smallOverviewW, smallOverviewH)),
        Color.black.r,
        Color.black.g,
        Color.black.b,
        smallOverviewOuterBorderMm))
    gridInstr ++ greyInstrs ++ layoutGridDashed ++ frameInstr ++ borderInstr
  }
}
