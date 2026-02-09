package clemniem.common

import cats.effect.IO
import clemniem.{Color, GridConfig, GridPart, Pixel, PixelPic}
import clemniem.common.pdf.{Instruction, JsPDF, PdfLayout, PdfLayoutConfig}
import clemniem.common.pdf.PdfLayoutConfig.SwatchBlock

/** Request for the book PDF: title, optional mosaic (pic + grid), step size, page background color, printer margin (mm; stays white), and optional layout config. */
final case class PrintBookRequest(
    title: String,
    mosaicPicAndGridOpt: Option[(PixelPic, GridConfig)],
    stepSizePx: Int = 16,
    pageBackgroundColor: Color = PdfUtils.defaultPageBackgroundColor,
    printerMarginMm: Double = 3.0,
    layoutConfig: Option[PdfLayoutConfig] = None
)

/** High-level PDF helpers. Build [[Instruction]]s and run them via [[JsPDF]]. */
object PdfUtils {

  /** Default page background: light pastel yellow (old LEGO-catalog style). */
  val defaultPageBackgroundColor: Color = Color.defaultPageBackground


  /** Extra offset (mm) applied below config so the top line of pixel counts (and content aligned with it) is shifted down on all overview and step pages. */
  private val pixelCountTopExtraMm = 2.0

  /** Layout params for the small overview: (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH). */
  private def smallOverviewLayoutParams(
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

  private val smallOverviewGreyOpacity   = 0.4
  private val smallOverviewHighlightFrameMm = 0.25
  private val smallOverviewOuterBorderMm  = 0.35
  private val explodedPartFrameLineWidthMm = 0.25
  private val smallOverviewStepGridLineWidthMm = 0.15
  private val smallOverviewStepGridDashMm      = 0.9
  private val smallOverviewStepGridGapMm       = 1.0
  private val progressBarHeightMm              = 3.0

  /** Stroke rects for layout grid over full mosaic (small overview): lines at grid part boundaries, not step subdivisions. */
  private def smallOverviewLayoutGridStrokeRects(
      x0: Double,
      y0: Double,
      imageW: Double,
      imageH: Double,
      grid: GridConfig,
      mosaicWidthPx: Int,
      mosaicHeightPx: Int,
      lineWidthMm: Double
  ): List[(Double, Double, Double, Double)] = {
    val half    = lineWidthMm / 2
    val parts   = grid.parts.toList
    val scaleX  = imageW / mosaicWidthPx
    val scaleY  = imageH / mosaicHeightPx
    val allX    = parts.flatMap(p => List(p.x, p.x + p.width)).distinct.sorted
    val allY    = parts.flatMap(p => List(p.y, p.y + p.height)).distinct.sorted
    val interiorX = allX.drop(1).dropRight(1)
    val interiorY = allY.drop(1).dropRight(1)
    val verticals   = interiorX.map { xPx =>
      val x = x0 + xPx * scaleX - half
      (x, y0, lineWidthMm, imageH)
    }
    val horizontals = interiorY.map { yPx =>
      val y = y0 + yPx * scaleY - half
      (x0, y, imageW, lineWidthMm)
    }
    verticals ++ horizontals
  }

  /** Draw the small overview: full mosaic, grey overlays on greyRectsMm (everything except highlighted), dashed layout grid (part boundaries), black frame on highlightedRectMm, then outer border. */
  private def drawSmallOverview(
      fullPic: PixelPic,
      smallX0: Double,
      smallY0: Double,
      smallOverviewW: Double,
      smallOverviewH: Double,
      greyRectsMm: List[(Double, Double, Double, Double)],
      highlightedRectMm: (Double, Double, Double, Double),
      grid: GridConfig
  ): List[Instruction] = {
    val (_, _, fullRgbFlat) = pixelPicToRgbFlat(fullPic)
    val gridInstr   = List(Instruction.DrawPixelGrid(smallX0, smallY0, smallOverviewW, smallOverviewH, fullPic.width, fullPic.height, fullRgbFlat))
    val greyInstrs  = greyRectsMm.map { case (x, y, w, h) => Instruction.FillRectWithOpacity(x, y, w, h, Color.smallOverviewGrey.r, Color.smallOverviewGrey.g, Color.smallOverviewGrey.b, smallOverviewGreyOpacity) }
    val layoutGridRects = smallOverviewLayoutGridStrokeRects(smallX0, smallY0, smallOverviewW, smallOverviewH, grid, fullPic.width, fullPic.height, smallOverviewStepGridLineWidthMm)
    val layoutGridDashed = dashedStrokeRectsInstructions(layoutGridRects, smallOverviewStepGridDashMm, smallOverviewStepGridGapMm)
    val frameInstr  = List(Instruction.DrawStrokeRects(List(highlightedRectMm), Color.black.r, Color.black.g, Color.black.b, smallOverviewHighlightFrameMm))
    val borderInstr = List(Instruction.DrawStrokeRects(List((smallX0, smallY0, smallOverviewW, smallOverviewH)), Color.black.r, Color.black.g, Color.black.b, smallOverviewOuterBorderMm))
    gridInstr ++ greyInstrs ++ layoutGridDashed ++ frameInstr ++ borderInstr
  }

  /** Progress bar at the bottom of the page, above the printer margin: full width (0 to pageW), y = pageH - marginTB - bar height. For booklet: bar represents overall progress 0–100%; when filling the second half, the first half is always completely full. */
  private def progressBarInstructions(
      pageIndex1Based: Int,
      totalPages: Int,
      pageW: Double,
      pageH: Double,
      marginTB: Double
  ): List[Instruction] = {
    if (totalPages <= 0) Nil
    else {
      val barY = pageH - marginTB - progressBarHeightMm
      if (barY < 0) Nil
      else {
        val barX     = 0.0
        val barW     = pageW
        val progress = pageIndex1Based.toDouble / totalPages
        val fillRatio = math.min(1.0, progress)
        val fillW = (barW * fillRatio).max(0)
        val bg    = List(Instruction.FillRect(barX, barY, barW, progressBarHeightMm, Color.progressBarBackgroundPastelBlue.r, Color.progressBarBackgroundPastelBlue.g, Color.progressBarBackgroundPastelBlue.b))
        val fill  = if (fillW > 0) List(Instruction.FillRect(barX, barY, fillW, progressBarHeightMm, Color.progressBarFill.r, Color.progressBarFill.g, Color.progressBarFill.b)) else Nil
        bg ++ fill
      }
    }
  }

  /** Insert progress bar instructions before each AddPage (for the page we're leaving) and before Save (for the last page). Uses foldLeft to avoid stack overflow on large documents. */
  private def insertProgressBars(
      instructions: List[Instruction],
      totalPages: Int,
      pageW: Double,
      pageH: Double,
      marginTB: Double
  ): List[Instruction] = {
    type State = (Int, List[Instruction]) // currentPage, reversed result
    val (_, revResult) = instructions.foldLeft[State]((1, Nil)) { case ((currentPage, acc), inst) =>
      inst match {
        case Instruction.AddPage =>
          val bar = progressBarInstructions(currentPage, totalPages, pageW, pageH, marginTB)
          (currentPage + 1, Instruction.AddPage :: (bar.reverse ++ acc))
        case s @ Instruction.Save(_) =>
          val bar = progressBarInstructions(currentPage, totalPages, pageW, pageH, marginTB)
          (currentPage, s :: (bar.reverse ++ acc))
        case other =>
          (currentPage, other :: acc)
      }
    }
    revResult.reverse
  }

  /** Generate the book PDF. Single entry point for both Print PDF buttons; pass a [[PrintBookRequest]]. */
  def printBookPdf(request: PrintBookRequest): IO[Unit] = IO {
    val config = request.layoutConfig.getOrElse(PdfLayoutConfig.default)
    runPrintBookPdf(
      request.title,
      request.mosaicPicAndGridOpt,
      request.stepSizePx,
      request.pageBackgroundColor,
      request.printerMarginMm,
      config
    )
  }

  private def runPrintBookPdf(
      title: String,
      mosaicPicAndGridOpt: Option[(PixelPic, GridConfig)],
      stepSizePx: Int,
      pageBackgroundColor: Color,
      printerMarginMm: Double,
      config: PdfLayoutConfig
  ): Unit = {
    val (pageW, pageH) = (config.global.pageSizeMm, config.global.pageSizeMm)
    val marginLR       = printerMarginMm + config.global.contentPaddingLRMm
    val marginTB       = printerMarginMm + config.global.contentPaddingTBMm
    val availableW     = pageW - 2 * marginLR
    val availableH     = pageH - 2 * marginTB

    val (coverInstrs, afterCoverInstrs, chapterInstrs) = mosaicPicAndGridOpt match {
      case Some((pic, grid)) =>
        val cover        = coverWithMosaic(title, pic, pageW, pageH, marginLR, marginTB, availableW, config)
        val emptyPage    = List(Instruction.AddPage)
        val fullOverview = fullOverviewPageInstructions(pic, grid, marginLR, marginTB, availableW, availableH, config)
        val chapters     = allChaptersInstructions(pic, grid, marginLR, marginTB, availableW, availableH, stepSizePx, config)
        (cover, emptyPage ++ fullOverview, chapters)
      case None =>
        (PdfLayout.coverInstructions(title, printerMarginMm, config), List(Instruction.AddPage), Nil)
    }
    val rawInstructions = coverInstrs ++ afterCoverInstrs ++ chapterInstrs :+ Instruction.Save("mosaic-book.pdf")
    val totalPages     = 1 + rawInstructions.count { case Instruction.AddPage => true; case _ => false }
    val instructions   = insertProgressBars(rawInstructions, totalPages, pageW, pageH, marginTB)
    JsPDF.run(instructions, pageBackgroundColor, printerMarginMm)
  }

  /** Cover page: centered image with 3mm white frame + black outline, title in top-right over frame (NES-style). */
  private def coverWithMosaic(
      title: String,
      pic: PixelPic,
      pageW: Double,
      pageH: Double,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val c        = config.cover
    val availableH = pageH - 2 * marginTB
    val (pw, ph, rgbFlat) = pixelPicToRgbFlat(pic)
    val scale    = (availableW / pw).min(availableH / ph)
    val imageW   = pw * scale
    val imageH   = ph * scale
    val x0       = marginLR + (availableW - imageW) / 2
    val y0       = marginTB + (availableH - imageH) / 2
    val m        = c.frameWhiteMarginMm
    val frameX   = x0 - m
    val frameY   = y0 - m
    val frameW   = imageW + 2 * m
    val frameH   = imageH + 2 * m
    val titleXLeft = frameX + 5.0
    val titleYTop  = frameY - 4.0
    List(
      Instruction.PageSize(pageW, pageH),
      Instruction.RoundedFillRect(frameX, frameY, frameW, frameH, c.frameCornerRadiusMm, Color.white.r, Color.white.g, Color.white.b),
      Instruction.RoundedStrokeRect(frameX, frameY, frameW, frameH, c.frameCornerRadiusMm, Color.black.r, Color.black.g, Color.black.b, c.frameStrokeLineWidthMm),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat),
      Instruction.TextWithBackground(titleXLeft, titleYTop, title, c.titleFontSizePt, c.titleBoxPaddingMm, alignLeft = true, Color.white.r, Color.white.g, Color.white.b)
    )
  }

  /** One pixel-count row: swatch (with black frame) + "× count". Uses [[Instruction.DrawSwatchRow]] (text centered with swatch). */
  private def drawSwatchRow(x: Double, y: Double, r: Int, g: Int, b: Int, count: Int, sw: SwatchBlock): List[Instruction] =
    List(Instruction.DrawSwatchRow(x, y, r, g, b, count, sw.swatchSizeMm, sw.swatchGapMm, sw.countFontSizePt, sw.swatchStrokeLineWidthMm))

  /** Multiple pixel-count rows. Each row uses [[drawSwatchRow]]; y advances by sw.lineHeightMm (includes padding between rows). */
  private def drawSwatchRows(x: Double, yStart: Double, rows: Seq[(Int, Int, Int, Int)], sw: SwatchBlock): List[Instruction] =
    rows.zipWithIndex.flatMap { case ((r, g, b, count), i) =>
      drawSwatchRow(x, yStart + sw.firstLineOffsetMm + i * sw.lineHeightMm, r, g, b, count, sw)
    }.toList

  /** One page after the empty page: color list top-left, image right (top-aligned). If grid has parts, draw exploded view with gaps; else single image. No title. */
  private def fullOverviewPageInstructions(
      fullPic: PixelPic,
      grid: GridConfig,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val fo  = config.fullOverview
    val sw  = fo.swatch
    val contentTopY    = marginTB + fo.titleOffsetFromTopMm + pixelCountTopExtraMm
    val imageAreaW     = availableW - fo.colorListReservedWidthMm
    val imageAreaH     = availableH - fo.titleOffsetFromTopMm - pixelCountTopExtraMm
    val colorRows = fullPic.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
      val px = fullPic.paletteLookup(idx)
      (px.r, px.g, px.b, count)
    }
    val colorCountInstrs = drawSwatchRows(marginLR, contentTopY, colorRows, sw)
    val imageInstrs = if (grid.parts.nonEmpty) {
      explodedOverviewInstructions(
        fullPic,
        grid,
        marginLR + fo.colorListReservedWidthMm,
        contentTopY,
        imageAreaW,
        imageAreaH,
        fo.explodedGapMm,
        fo.explodedDimensionGapMm,
        fo.explodedDimensionFontSizePt,
        fo.explodedDimensionLineWidthMm,
        "center"
      )
    } else {
      val (pw, ph, rgbFlat) = pixelPicToRgbFlat(fullPic)
      val scale             = (imageAreaW / pw).min(imageAreaH / ph)
      val imageW            = pw * scale
      val imageH            = ph * scale
      val x0                = marginLR + fo.colorListReservedWidthMm + (imageAreaW - imageW) / 2
      val y0                = contentTopY
      List(Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat))
    }
    List(Instruction.AddPage) ++ imageInstrs ++ colorCountInstrs
  }

  /** Exploded overview: one DrawPixelGrid per grid part, gapMm between parts; dimension markings (architecture-style) on top and right. align: "left" | "center" | "right". */
  private def explodedOverviewInstructions(
      fullPic: PixelPic,
      grid: GridConfig,
      areaX: Double,
      areaY: Double,
      areaW: Double,
      areaH: Double,
      gapMm: Double,
      dimGapMm: Double,
      dimFontSizePt: Int,
      dimLineWidthMm: Double,
      align: String
  ): List[Instruction] = {
    val (gridInstrs, baseX, baseY, scale, totalWmm, colOffsetPx, rowOffsetPx, colWidthsPx, rowHeightsPx, uniqueXs, uniqueYs) =
      explodedOverviewLayoutAndGrid(fullPic, grid, areaX, areaY, areaW, areaH, gapMm, align)
    val topDimY = baseY - dimGapMm
    val topTextY = baseY - dimGapMm - 2.0
    val rightDimX = baseX + totalWmm + dimGapMm
    val rightTextX = rightDimX + 2.0
    val topDims = (0 until uniqueXs.size).flatMap { c =>
      val colStartX = baseX + colOffsetPx(c) * scale + c * gapMm
      val colEndX = colStartX + colWidthsPx(c) * scale
      val centerX = baseX + (colOffsetPx(c) + colWidthsPx(c) / 2.0) * scale + c * gapMm
      List(
        Instruction.DrawLine(colStartX, topDimY, colEndX, topDimY, dimLineWidthMm, Color.black.r, Color.black.g, Color.black.b),
        Instruction.TextAligned(centerX, topTextY, colWidthsPx(c).toString, "center", dimFontSizePt)
      )
    }
    val rightDims = (0 until uniqueYs.size).flatMap { r =>
      val rowStartY = baseY + rowOffsetPx(r) * scale + r * gapMm
      val rowEndY = rowStartY + rowHeightsPx(r) * scale
      val centerY = baseY + (rowOffsetPx(r) + rowHeightsPx(r) / 2.0) * scale + r * gapMm
      List(
        Instruction.DrawLine(rightDimX, rowStartY, rightDimX, rowEndY, dimLineWidthMm, Color.black.r, Color.black.g, Color.black.b),
        Instruction.TextAligned(rightTextX, centerY, rowHeightsPx(r).toString, "left", dimFontSizePt)
      )
    }
    gridInstrs ++ topDims ++ rightDims
  }

  /** Returns exploded grid instructions and layout (baseX, baseY, scale, totalWmm, colOffsetPx, rowOffsetPx, colWidthsPx, rowHeightsPx, uniqueXs, uniqueYs). align: "left" | "center" | "right". */
  private def explodedOverviewLayoutAndGrid(
      fullPic: PixelPic,
      grid: GridConfig,
      areaX: Double,
      areaY: Double,
      areaW: Double,
      areaH: Double,
      gapMm: Double,
      align: String
  ): (List[Instruction], Double, Double, Double, Double, List[Int], List[Int], List[Int], List[Int], List[Int], List[Int]) = {
    val parts = grid.parts.toList
    val uniqueXs = parts.map(_.x).distinct.sorted
    val uniqueYs = parts.map(_.y).distinct.sorted
    val colWidthsPx = uniqueXs.map { x => parts.filter(_.x == x).map(_.width).max }
    val rowHeightsPx = uniqueYs.map { y => parts.filter(_.y == y).map(_.height).max }
    val totalWpx = colWidthsPx.sum.toDouble
    val totalHpx = rowHeightsPx.sum.toDouble
    val numGapsX = (uniqueXs.size - 1).max(0)
    val numGapsY = (uniqueYs.size - 1).max(0)
    val scale = (
      (areaW - numGapsX * gapMm) / totalWpx
    ).min((areaH - numGapsY * gapMm) / totalHpx)
    val totalWmm = totalWpx * scale + numGapsX * gapMm
    val baseX = align match {
      case "right"  => areaX + areaW - totalWmm
      case "left"   => areaX
      case _        => areaX + (areaW - totalWmm) / 2
    }
    val baseY = areaY
    val colOffsetPx = colWidthsPx.scanLeft(0)(_ + _).dropRight(1)
    val rowOffsetPx = rowHeightsPx.scanLeft(0)(_ + _).dropRight(1)
    val (gridInstrs, partRects) = parts.foldLeft((List.empty[Instruction], List.empty[(Double, Double, Double, Double)])) { case ((accInstrs, accRects), part) =>
      val col = uniqueXs.indexOf(part.x)
      val row = uniqueYs.indexOf(part.y)
      val offsetXmm = colOffsetPx(col) * scale + col * gapMm
      val offsetYmm = rowOffsetPx(row) * scale + row * gapMm
      val x0 = baseX + offsetXmm
      val y0 = baseY + offsetYmm
      val wMm = part.width * scale
      val hMm = part.height * scale
      val rect = (x0, y0, wMm, hMm)
      fullPic.crop(part.x, part.y, part.width, part.height) match {
        case Some(cropped) =>
          val (_, _, rgbFlat) = pixelPicToRgbFlat(cropped)
          (accInstrs :+ Instruction.DrawPixelGrid(x0, y0, wMm, hMm, part.width, part.height, rgbFlat), accRects :+ rect)
        case None =>
          (accInstrs, accRects :+ rect)
      }
    }
    val frameInstrs = List(Instruction.DrawStrokeRects(partRects, Color.black.r, Color.black.g, Color.black.b, explodedPartFrameLineWidthMm))
    (gridInstrs ++ frameInstrs, baseX, baseY, scale, totalWmm, colOffsetPx, rowOffsetPx, colWidthsPx, rowHeightsPx, uniqueXs, uniqueYs)
  }

  /** All chapters: one chapter per plate (overview page + sections per step). */
  private def allChaptersInstructions(
      fullPic: PixelPic,
      grid: GridConfig,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      stepSizePx: Int,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val parts = grid.parts
    parts.indices.toList.flatMap { idx =>
      chapterInstructionsForPlate(idx + 1, parts(idx), fullPic, grid, marginLR, marginTB, availableW, availableH, stepSizePx, config)
    }
  }

  /** One chapter (one plate): overview page (left = colors + small grid overview, right = exploded plate with dimensions) + sections per build step. No title. */
  private def chapterInstructionsForPlate(
      plateIndex: Int,
      part: GridPart,
      fullPic: PixelPic,
      grid: GridConfig,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      stepSizePx: Int,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val co  = config.chapterOverview
    val sw  = co.swatch
    val parts = grid.parts
    val platePicOpt = fullPic.crop(part.x, part.y, part.width, part.height)
    if (platePicOpt.isEmpty) Nil
    else {
      val platePic = platePicOpt.get
      val contentTopY = marginTB + co.contentTopOffsetFromTopMm + pixelCountTopExtraMm
      val colorRows = platePic.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
        val px = fullPic.paletteLookup(idx)
        (px.r, px.g, px.b, count)
      }
      val colorCountInstrs = drawSwatchRows(marginLR, contentTopY, colorRows, sw)
      val colorListBottom = contentTopY + sw.firstLineOffsetMm + colorRows.size * sw.lineHeightMm
      val gridOverviewY   = colorListBottom + co.gridOverviewGapAboveMm
      val (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH) = smallOverviewLayoutParams(fullPic, gridOverviewY, marginLR, co)
      val greyRectsMm     = parts.toList.filter(_ != part).map(p => (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
      val highlightedRectMm = (smallX0 + part.x * smallScale, smallY0 + part.y * smallScale, part.width * smallScale, part.height * smallScale)
      val smallOverviewInstrs = drawSmallOverview(fullPic, smallX0, smallY0, smallOverviewW, smallOverviewH, greyRectsMm, highlightedRectMm, grid)
      val areaX = marginLR + co.colorListReservedWidthMm
      val areaY = contentTopY
      val areaW = availableW - co.colorListReservedWidthMm
      val areaH = availableH - co.contentTopOffsetFromTopMm - pixelCountTopExtraMm
      val rightAreaW = areaW * co.explodedAreaScaleFactor
      val rightAreaH = areaH * co.explodedAreaScaleFactor
      val rightAreaX = areaX + areaW - rightAreaW
      val nColsStepPlate = platePic.width / stepSizePx
      val nRowsStepPlate = platePic.height / stepSizePx
      val stepGridForPlate =
        if (nColsStepPlate >= 1 && nRowsStepPlate >= 1) {
          val stepParts = (0 until nRowsStepPlate).flatMap { cy =>
            (0 until nColsStepPlate).map { cx =>
              GridPart(cx * stepSizePx, cy * stepSizePx, stepSizePx, stepSizePx)
            }
          }
          GridConfig(nColsStepPlate, nRowsStepPlate, stepParts.toArray)
        } else {
          GridConfig(1, 1, Array(GridPart(0, 0, platePic.width, platePic.height)))
        }
      val explodedInstrs = explodedOverviewInstructions(platePic, stepGridForPlate, rightAreaX, areaY, rightAreaW, rightAreaH, co.explodedGapMm, co.explodedDimensionGapMm, co.explodedDimensionFontSizePt, co.explodedDimensionLineWidthMm, "right")
      val allPlatesDivisible = parts.forall(p => p.width % stepSizePx == 0 && p.height % stepSizePx == 0)
      val divisibilityNote = if (allPlatesDivisible) Nil else {
        val noteY = gridOverviewY + smallOverviewH + 4.0
        List(
          Instruction.FontSize(co.divisibilityNoteFontSizePt),
          Instruction.Text(marginLR, noteY, s"Note: some sections don't match the step size ($stepSizePx); only matching sections get step-by-step pages.")
        )
      }
      val chapterOverviewPage = List(Instruction.AddPage) ++ colorCountInstrs ++ smallOverviewInstrs ++ divisibilityNote ++ explodedInstrs

      val thisPlateDivisible = platePic.width % stepSizePx == 0 && platePic.height % stepSizePx == 0
      val sectionInstrs = if (!thisPlateDivisible) {
        val msg = s"Section $plateIndex: size (${platePic.width}×${platePic.height}) doesn't work with the step size ($stepSizePx). Step-by-step pages are skipped for it."
        List(Instruction.AddPage, Instruction.FontSize(co.nonDivisibleMessageFontSizePt), Instruction.Text(marginLR, marginTB + co.nonDivisibleMessageOffsetFromTopMm, msg))
      } else {
        sectionInstructionsForPlate(platePic, fullPic, grid, part, stepSizePx, marginLR, marginTB, availableW, availableH, config)
      }
      chapterOverviewPage ++ sectionInstrs
    }
  }

  /** One section per step: layered canvases (4 per page) for each step patch. */
  private def sectionInstructionsForPlate(
      platePic: PixelPic,
      fullPic: PixelPic,
      grid: GridConfig,
      part: GridPart,
      stepSizePx: Int,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val nCols        = platePic.width / stepSizePx
    val nRows        = platePic.height / stepSizePx
    (0 until nRows).flatMap { cy =>
      (0 until nCols).map { cx =>
        val stepX   = cx * stepSizePx
        val stepY   = cy * stepSizePx
        platePic.crop(stepX, stepY, stepSizePx, stepSizePx) match {
          case Some(patch) =>
            layerPagesForPatch(patch, fullPic, stepSizePx, cx, cy, part, grid, marginLR, marginTB, availableW, availableH, config)
          case None =>
            Nil
        }
      }
    }.toList.flatten
  }

  /** Left column for a step page: patch color counts, small full-mosaic overview with non-current plates and other steps greyed (only current step in full color). */
  private def stepPageLeftColumnInstructions(
      fullPic: PixelPic,
      grid: GridConfig,
      part: GridPart,
      patch: PixelPic,
      stepCx: Int,
      stepCy: Int,
      stepSizePx: Int,
      marginLR: Double,
      marginTB: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val co            = config.chapterOverview
    val sw            = co.swatch
    val parts         = grid.parts
    val contentTopY   = marginTB + co.contentTopOffsetFromTopMm + pixelCountTopExtraMm
    val colorRows     = patch.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
      val px = fullPic.paletteLookup(idx)
      (px.r, px.g, px.b, count)
    }
    val colorCountInstrs = drawSwatchRows(marginLR, contentTopY, colorRows, sw)
    val colorListBottom  = contentTopY + sw.firstLineOffsetMm + colorRows.size * sw.lineHeightMm
    val gridOverviewY    = colorListBottom + co.gridOverviewGapAboveMm
    val (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH) = smallOverviewLayoutParams(fullPic, gridOverviewY, marginLR, co)
    val greyRectsOtherPlates = parts.toList.filter(_ != part).map(p => (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
    val nColsPlate = part.width / stepSizePx
    val nRowsPlate = part.height / stepSizePx
    val greyRectsOtherSteps = (0 until nRowsPlate).flatMap { cy =>
      (0 until nColsPlate).filter(cx => cx != stepCx || cy != stepCy).map { cx =>
        val gx = part.x + cx * stepSizePx
        val gy = part.y + cy * stepSizePx
        (smallX0 + gx * smallScale, smallY0 + gy * smallScale, stepSizePx * smallScale, stepSizePx * smallScale)
      }
    }.toList
    val greyRectsMm       = greyRectsOtherPlates ++ greyRectsOtherSteps
    val stepGlobalX       = part.x + stepCx * stepSizePx
    val stepGlobalY       = part.y + stepCy * stepSizePx
    val highlightedRectMm = (smallX0 + stepGlobalX * smallScale, smallY0 + stepGlobalY * smallScale, stepSizePx * smallScale, stepSizePx * smallScale)
    val smallOverviewInstrs = drawSmallOverview(fullPic, smallX0, smallY0, smallOverviewW, smallOverviewH, greyRectsMm, highlightedRectMm, grid)
    colorCountInstrs ++ smallOverviewInstrs
  }

  /** Layered canvases for one step's patch: cumulative colors (least→most), 4 per page, 16×16 and 4×4 grids. Left column = patch counts + small overview + step marker. */
  private def layerPagesForPatch(
      patch: PixelPic,
      fullPic: PixelPic,
      stepSizePx: Int,
      stepCx: Int,
      stepCy: Int,
      part: GridPart,
      grid: GridConfig,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val lp          = config.layerPage
    val co          = config.chapterOverview
    val contentTopY = marginTB + co.contentTopOffsetFromTopMm + pixelCountTopExtraMm
    val rightAreaX      = marginLR + co.colorListReservedWidthMm
    val rightAreaW      = availableW - co.colorListReservedWidthMm
    val rightAreaY      = contentTopY
    val rightAreaH      = availableH - co.contentTopOffsetFromTopMm - pixelCountTopExtraMm
    val layerGridScale  = 0.85
    val effectiveW      = rightAreaW * layerGridScale
    val effectiveH      = rightAreaH * layerGridScale
    val patchRows       = (lp.patchesPerPage + lp.patchGridCols - 1) / lp.patchGridCols
    val patchCellMm     = (
      (effectiveW - (lp.patchGridCols - 1) * lp.patchGapMm) / lp.patchGridCols
    ).min((effectiveH - (patchRows - 1) * lp.patchGapMm) / patchRows)
    val totalGridW      = lp.patchGridCols * patchCellMm + (lp.patchGridCols - 1) * lp.patchGapMm
    val startX          = rightAreaX + rightAreaW - totalGridW
    val startY          = rightAreaY
    val colorIndicesAsc = patch.getIndexByCount
    val (_, layerRgbFlats) = colorIndicesAsc.foldLeft((Set.empty[Int], Vector.empty[Vector[Int]])) {
      case ((set, acc), idx) =>
        val newSet = set + idx
        (newSet, acc :+ buildCumulativeLayerRgb(patch, newSet, (i: Int) => fullPic.paletteLookup(i)))
    }
    val layerRgbFlatsList = layerRgbFlats.toList
    val patchW = patch.width
    val patchH = patch.height
    val leftColumnInstrs = stepPageLeftColumnInstructions(fullPic, grid, part, patch, stepCx, stepCy, stepSizePx, marginLR, marginTB, config)
    layerRgbFlatsList.grouped(lp.patchesPerPage).toList.zipWithIndex.flatMap { case (layerBatch, batchIdx) =>
      val scale  = (patchCellMm / patchW).min(patchCellMm / patchH)
      val imageW = patchW * scale
      val imageH = patchH * scale
      val perPage = layerBatch.zipWithIndex.map { case (rgbFlat, i) =>
        val col     = i % lp.patchGridCols
        val row     = i / lp.patchGridCols
        val cellX   = startX + col * (patchCellMm + lp.patchGapMm)
        val cellY   = startY + row * (patchCellMm + lp.patchGapMm)
        val x0      = cellX + (patchCellMm - imageW) / 2
        val y0      = cellY + (patchCellMm - imageH) / 2
        (x0, y0, imageW, imageH, rgbFlat)
      }
      val patchBorderLineWidthMm = 0.35
      List(Instruction.AddPage) ++ leftColumnInstrs ++ perPage.flatMap { case (x0, y0, w, h, rgb) =>
        val grid16Rects   = grid16x16PatchStrokeRects(x0, y0, w, h, patchW, patchH, stepSizePx, lp.grid16LineWidthMm)
        val grid4x4Rects  = grid4x4StrokeRects(x0, y0, w, h, lp.grid4x4LineWidthMm)
        val grid16Dashed  = dashedStrokeRectsInstructions(grid16Rects, 0.9, 1.0)
        val grid4x4Dashed = dashedStrokeRectsInstructions(grid4x4Rects, 0.9, 1.0)
        val borderInstr   = Instruction.DrawStrokeRects(List((x0, y0, w, h)), Color.black.r, Color.black.g, Color.black.b, patchBorderLineWidthMm)
        List(Instruction.DrawPixelGrid(x0, y0, w, h, patchW, patchH, rgb)) ++ grid16Dashed ++ grid4x4Dashed :+ borderInstr
      }
    }
  }

  /** Instructions to draw a line as white full line with dashed black on top (for step grid). (x1,y1) to (x2,y2), lineWidthMm, dash and gap in mm. Includes a final partial dash so black runs to the end. */
  private def dashedLineInstructions(
      x1: Double, y1: Double, x2: Double, y2: Double,
      lineWidthMm: Double,
      dashLengthMm: Double,
      gapMm: Double
  ): List[Instruction] = {
    val length = math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    if (length <= 0) Nil
    else {
      val white = Instruction.DrawLine(x1, y1, x2, y2, lineWidthMm, Color.white.r, Color.white.g, Color.white.b)
      val period = dashLengthMm + gapMm
      val nFullDashes = (length / period).floor.toInt.max(0)
      val fullBlackDashes = (0 until nFullDashes).flatMap { i =>
        val startMm = i * period
        val endMm   = (i * period + dashLengthMm).min(length)
        if (endMm <= startMm) Nil
        else {
          val startT = startMm / length
          val endT   = endMm / length
          val sx = x1 + startT * (x2 - x1)
          val sy = y1 + startT * (y2 - y1)
          val ex = x1 + endT * (x2 - x1)
          val ey = y1 + endT * (y2 - y1)
          List(Instruction.DrawLine(sx, sy, ex, ey, lineWidthMm, Color.black.r, Color.black.g, Color.black.b))
        }
      }.toList
      val remainderStart = nFullDashes * period
      val finalDash =
        if (remainderStart < length && dashLengthMm > 0) {
          val startT = remainderStart / length
          val ex = x1 + 1.0 * (x2 - x1)
          val ey = y1 + 1.0 * (y2 - y1)
          val sx = x1 + startT * (x2 - x1)
          val sy = y1 + startT * (y2 - y1)
          List(Instruction.DrawLine(sx, sy, ex, ey, lineWidthMm, Color.black.r, Color.black.g, Color.black.b))
        } else Nil
      white :: (fullBlackDashes ++ finalDash)
    }
  }

  /** Convert stroke rects (thin lines as rects) to dashed white+black line instructions. Each rect (x,y,w,h): if w>=h horizontal else vertical; line thickness from rect. */
  private def dashedStrokeRectsInstructions(
      rects: List[(Double, Double, Double, Double)],
      dashLengthMm: Double,
      gapMm: Double
  ): List[Instruction] = {
    val dashLen = if (dashLengthMm > 0) dashLengthMm else 1.5
    val gap    = if (gapMm >= 0) gapMm else 1.0
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

  /** Stroke rects for a 4×4 grid overlay: 3 vertical + 3 horizontal lines. lineWidthMm should be small (e.g. 0.1) for thin strokes. */
  private def grid4x4StrokeRects(x0: Double, y0: Double, imageW: Double, imageH: Double, lineWidthMm: Double): List[(Double, Double, Double, Double)] = {
    val half      = lineWidthMm / 2
    val cellW     = imageW / 4
    val cellH     = imageH / 4
    val verticals   = (1 to 3).map { i => (x0 + i * cellW - half, y0, lineWidthMm, imageH) }.toList
    val horizontals = (1 to 3).map { i => (x0, y0 + i * cellH - half, imageW, lineWidthMm) }.toList
    verticals ++ horizontals
  }

  /** Stroke rects for 16×16 step grid: one line every patchSizePx pixels in plate space. */
  private def grid16x16PatchStrokeRects(
      x0: Double,
      y0: Double,
      imageW: Double,
      imageH: Double,
      plateWidth: Int,
      plateHeight: Int,
      patchSizePx: Int,
      lineWidthMm: Double
  ): List[(Double, Double, Double, Double)] = {
    val half      = lineWidthMm / 2
    val nCols     = plateWidth / patchSizePx
    val nRows     = plateHeight / patchSizePx
    val pxW       = imageW / plateWidth
    val pxH       = imageH / plateHeight
    val verticals   = (1 until nCols).map { i =>
      val x = x0 + i * patchSizePx * pxW - half
      (x, y0, lineWidthMm, imageH)
    }.toList
    val horizontals = (1 until nRows).map { j =>
      val y = y0 + j * patchSizePx * pxH - half
      (x0, y, imageW, lineWidthMm)
    }.toList
    verticals ++ horizontals
  }

  /** Full-resolution RGB flat for the plate: pixels in cumulative color set get their color, rest get layer background (like BuildScreen preview per-pixel). */
  private def buildCumulativeLayerRgb(platePic: PixelPic, colorIndexSet: Set[Int], paletteLookup: Int => Pixel): Vector[Int] =
    platePic.pixels.iterator.flatMap { idx =>
      val (r, g, b) =
        if (colorIndexSet.contains(idx)) {
          val p = paletteLookup(idx)
          (p.r, p.g, p.b)
        } else Color.layerPatchBackground.rgb
      Vector(r, g, b)
    }.toVector

  /** Row-major flat RGB (3 ints per pixel) for use in DrawPixelGrid. */
  private def pixelPicToRgbFlat(pic: PixelPic): (Int, Int, Vector[Int]) = {
    val flat = pic.pixels.iterator.flatMap { i =>
      val p = pic.paletteLookup(i)
      Vector(p.r, p.g, p.b)
    }.toVector
    (pic.width, pic.height, flat)
  }
}
