package clemniem.common.pdf

import clemniem.{Color, GridPart, Layout, PixelPic}

/** Page-level instruction generators for the PDF book: cover, overviews, chapters, and layer pages. */
private[common] object PdfPageBuilders {

  /** Cover page: centered image with 3mm white frame + black outline, title in top-left over frame (NES-style). */
  def coverWithMosaic(
    title: String,
    pic: PixelPic,
    pageW: Double,
    pageH: Double,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val c                 = config.cover
    val availableH        = pageH - 2 * marginTB
    val (pw, ph, rgbFlat) = PdfDrawHelpers.pixelPicToRgbFlat(pic)
    val scale             = (availableW / pw).min(availableH / ph)
    val imageW            = pw * scale
    val imageH            = ph * scale
    val x0                = marginLR + (availableW - imageW) / 2
    val y0                = marginTB + (availableH - imageH) / 2
    val m                 = c.frameWhiteMarginMm
    val frameX            = x0 - m
    val frameY            = y0 - m
    val frameW            = imageW + 2 * m
    val frameH            = imageH + 2 * m
    val titleXLeft        = frameX + 5.0
    val titleYTop         = frameY - 4.0
    List(
      Instruction.PageSize(pageW, pageH),
      Instruction.RoundedFillRect(
        frameX,
        frameY,
        frameW,
        frameH,
        c.frameCornerRadiusMm,
        Color.white.r,
        Color.white.g,
        Color.white.b),
      Instruction.RoundedStrokeRect(
        frameX,
        frameY,
        frameW,
        frameH,
        c.frameCornerRadiusMm,
        Color.black.r,
        Color.black.g,
        Color.black.b,
        c.frameStrokeLineWidthMm),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat),
      Instruction.TextWithBackground(
        titleXLeft,
        titleYTop,
        title,
        c.titleFontSizePt,
        c.titleBoxPaddingMm,
        alignLeft = true,
        Color.white.r,
        Color.white.g,
        Color.white.b)
    )
  }

  /** Cover image only (no PageSize, no title): used for page 2 (back of front cover) and last page (back cover). */
  def coverMosaicImageOnly(
    pic: PixelPic,
    pageH: Double,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val c                 = config.cover
    val availableH        = pageH - 2 * marginTB
    val (pw, ph, rgbFlat) = PdfDrawHelpers.pixelPicToRgbFlat(pic)
    val scale             = (availableW / pw).min(availableH / ph)
    val imageW            = pw * scale
    val imageH            = ph * scale
    val x0                = marginLR + (availableW - imageW) / 2
    val y0                = marginTB + (availableH - imageH) / 2
    val m                 = c.frameWhiteMarginMm
    val frameX            = x0 - m
    val frameY            = y0 - m
    val frameW            = imageW + 2 * m
    val frameH            = imageH + 2 * m
    List(
      Instruction.RoundedFillRect(
        frameX,
        frameY,
        frameW,
        frameH,
        c.frameCornerRadiusMm,
        Color.white.r,
        Color.white.g,
        Color.white.b),
      Instruction.RoundedStrokeRect(
        frameX,
        frameY,
        frameW,
        frameH,
        c.frameCornerRadiusMm,
        Color.black.r,
        Color.black.g,
        Color.black.b,
        c.frameStrokeLineWidthMm),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat)
    )
  }

  /** One page after the back-of-cover page: color list top-left, image right (top-aligned). If grid has parts, draw
    * exploded view with gaps; else single image. No title.
    */
  def fullOverviewPageInstructions(
    fullPic: PixelPic,
    grid: Layout,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    availableH: Double,
    contentTopOffsetMm: Double,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val fo          = config.fullOverview
    val sw          = fo.swatch
    val contentTopY = marginTB + fo.titleOffsetFromTopMm + contentTopOffsetMm
    val imageAreaW  = availableW - fo.colorListReservedWidthMm
    val imageAreaH  = availableH - fo.titleOffsetFromTopMm - contentTopOffsetMm
    val colorRows = fullPic.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
      val px = fullPic.paletteLookup(idx)
      (px.r, px.g, px.b, count)
    }
    val colorCountInstrs = PdfDrawHelpers.drawSwatchRows(marginLR, contentTopY, colorRows, sw)
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
      val (pw, ph, rgbFlat) = PdfDrawHelpers.pixelPicToRgbFlat(fullPic)
      val scale             = (imageAreaW / pw).min(imageAreaH / ph)
      val imageW            = pw * scale
      val imageH            = ph * scale
      val x0                = marginLR + fo.colorListReservedWidthMm + (imageAreaW - imageW) / 2
      val y0                = contentTopY
      List(Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat))
    }
    List(Instruction.AddPage) ++ imageInstrs ++ colorCountInstrs
  }

  /** Exploded overview: one DrawPixelGrid per grid part, gapMm between parts; dimension markings (architecture-style)
    * on top and right. align: "left" | "center" | "right".
    */
  def explodedOverviewInstructions(
    fullPic: PixelPic,
    grid: Layout,
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
    val (
      gridInstrs,
      baseX,
      baseY,
      scale,
      totalWmm,
      colOffsetPx,
      rowOffsetPx,
      colWidthsPx,
      rowHeightsPx,
      uniqueXs,
      uniqueYs) =
      explodedOverviewLayoutAndGrid(fullPic, grid, areaX, areaY, areaW, areaH, gapMm, align)
    val topDimY    = baseY - dimGapMm
    val topTextY   = baseY - dimGapMm - 2.0
    val rightDimX  = baseX + totalWmm + dimGapMm
    val rightTextX = rightDimX + 2.0
    val topDims = (0 until uniqueXs.size).flatMap { c =>
      val colStartX = baseX + colOffsetPx(c) * scale + c * gapMm
      val colEndX   = colStartX + colWidthsPx(c) * scale
      val centerX   = baseX + (colOffsetPx(c) + colWidthsPx(c) / 2.0) * scale + c * gapMm
      List(
        Instruction.DrawLine(
          colStartX,
          topDimY,
          colEndX,
          topDimY,
          dimLineWidthMm,
          Color.black.r,
          Color.black.g,
          Color.black.b),
        Instruction.TextAligned(centerX, topTextY, colWidthsPx(c).toString, "center", dimFontSizePt)
      )
    }
    val rightDims = (0 until uniqueYs.size).flatMap { r =>
      val rowStartY = baseY + rowOffsetPx(r) * scale + r * gapMm
      val rowEndY   = rowStartY + rowHeightsPx(r) * scale
      val centerY   = baseY + (rowOffsetPx(r) + rowHeightsPx(r) / 2.0) * scale + r * gapMm
      List(
        Instruction.DrawLine(
          rightDimX,
          rowStartY,
          rightDimX,
          rowEndY,
          dimLineWidthMm,
          Color.black.r,
          Color.black.g,
          Color.black.b),
        Instruction.TextAligned(rightTextX, centerY, rowHeightsPx(r).toString, "left", dimFontSizePt)
      )
    }
    gridInstrs ++ topDims ++ rightDims
  }

  /** Returns exploded grid instructions and layout (baseX, baseY, scale, totalWmm, colOffsetPx, rowOffsetPx,
    * colWidthsPx, rowHeightsPx, uniqueXs, uniqueYs). align: "left" | "center" | "right".
    */
  private def explodedOverviewLayoutAndGrid(
    fullPic: PixelPic,
    grid: Layout,
    areaX: Double,
    areaY: Double,
    areaW: Double,
    areaH: Double,
    gapMm: Double,
    align: String
  ): (
    List[Instruction],
    Double,
    Double,
    Double,
    Double,
    List[Int],
    List[Int],
    List[Int],
    List[Int],
    List[Int],
    List[Int]
  ) = {
    val parts        = grid.parts.toList
    val uniqueXs     = parts.map(_.x).distinct.sorted
    val uniqueYs     = parts.map(_.y).distinct.sorted
    val colWidthsPx  = uniqueXs.map(x => parts.filter(_.x == x).map(_.width).max)
    val rowHeightsPx = uniqueYs.map(y => parts.filter(_.y == y).map(_.height).max)
    val totalWpx     = colWidthsPx.sum.toDouble
    val totalHpx     = rowHeightsPx.sum.toDouble
    val numGapsX     = (uniqueXs.size - 1).max(0)
    val numGapsY     = (uniqueYs.size - 1).max(0)
    val scale = (
      (areaW - numGapsX * gapMm) / totalWpx
    ).min((areaH - numGapsY * gapMm) / totalHpx)
    val totalWmm = totalWpx * scale + numGapsX * gapMm
    val baseX = align match {
      case "right" => areaX + areaW - totalWmm
      case "left"  => areaX
      case _       => areaX + (areaW - totalWmm) / 2
    }
    val baseY       = areaY
    val colOffsetPx = colWidthsPx.scanLeft(0)(_ + _).dropRight(1)
    val rowOffsetPx = rowHeightsPx.scanLeft(0)(_ + _).dropRight(1)
    val (gridInstrs, partRects) =
      parts.foldLeft((List.empty[Instruction], List.empty[(Double, Double, Double, Double)])) {
        case ((accInstrs, accRects), part) =>
          val col       = uniqueXs.indexOf(part.x)
          val row       = uniqueYs.indexOf(part.y)
          val offsetXmm = colOffsetPx(col) * scale + col * gapMm
          val offsetYmm = rowOffsetPx(row) * scale + row * gapMm
          val x0        = baseX + offsetXmm
          val y0        = baseY + offsetYmm
          val wMm       = part.width * scale
          val hMm       = part.height * scale
          val rect      = (x0, y0, wMm, hMm)
          fullPic.crop(part.x, part.y, part.width, part.height) match {
            case Some(cropped) =>
              val (_, _, rgbFlat) = PdfDrawHelpers.pixelPicToRgbFlat(cropped)
              (
                accInstrs :+ Instruction.DrawPixelGrid(x0, y0, wMm, hMm, part.width, part.height, rgbFlat),
                accRects :+ rect)
            case None =>
              (accInstrs, accRects :+ rect)
          }
      }
    val frameInstrs = List(
      Instruction.DrawStrokeRects(
        partRects,
        Color.black.r,
        Color.black.g,
        Color.black.b,
        PdfDrawHelpers.explodedPartFrameLineWidthMm))
    (
      gridInstrs ++ frameInstrs,
      baseX,
      baseY,
      scale,
      totalWmm,
      colOffsetPx,
      rowOffsetPx,
      colWidthsPx,
      rowHeightsPx,
      uniqueXs,
      uniqueYs)
  }

  /** All chapters: one chapter per section (overview page + steps per section). */
  def allChaptersInstructions(
    fullPic: PixelPic,
    grid: Layout,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    availableH: Double,
    stepSizePx: Int,
    contentTopOffsetMm: Double,
    patchBgColor: Color,
    stacked: Boolean,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val parts = grid.parts
    parts.indices.toList.flatMap { idx =>
      chapterInstructionsForSection(
        idx + 1,
        parts(idx),
        fullPic,
        grid,
        marginLR,
        marginTB,
        availableW,
        availableH,
        stepSizePx,
        contentTopOffsetMm,
        patchBgColor,
        stacked,
        config)
    }
  }

  /** One chapter (one section): overview page (left = colors + small grid overview, right = exploded section with
    * dimensions) + steps per section. No title.
    */
  private def chapterInstructionsForSection(
    sectionIndex: Int,
    part: GridPart,
    fullPic: PixelPic,
    grid: Layout,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    availableH: Double,
    stepSizePx: Int,
    contentTopOffsetMm: Double,
    patchBgColor: Color,
    stacked: Boolean,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val co            = config.chapterOverview
    val sw            = co.swatch
    val parts         = grid.parts
    val sectionPicOpt = fullPic.crop(part.x, part.y, part.width, part.height)
    if (sectionPicOpt.isEmpty) Nil
    else {
      val sectionPic  = sectionPicOpt.get
      val contentTopY = marginTB + co.contentTopOffsetFromTopMm + contentTopOffsetMm
      val colorRows = sectionPic.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
        val px = fullPic.paletteLookup(idx)
        (px.r, px.g, px.b, count)
      }
      val colorCountInstrs = PdfDrawHelpers.drawSwatchRows(marginLR, contentTopY, colorRows, sw)
      val colorListBottom  = contentTopY + sw.firstLineOffsetMm + colorRows.size * sw.lineHeightMm
      val gridOverviewY    = colorListBottom + co.gridOverviewGapAboveMm
      val (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH) =
        PdfDrawHelpers.smallOverviewLayoutParams(fullPic, gridOverviewY, marginLR, co)
      val greyRectsMm = parts.toList
        .filter(_ != part)
        .map(p =>
          (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
      val highlightedRectMm = (
        smallX0 + part.x * smallScale,
        smallY0 + part.y * smallScale,
        part.width * smallScale,
        part.height * smallScale)
      val smallOverviewInstrs = PdfDrawHelpers.drawSmallOverview(
        fullPic,
        smallX0,
        smallY0,
        smallOverviewW,
        smallOverviewH,
        greyRectsMm,
        highlightedRectMm,
        grid)
      val areaX            = marginLR + co.colorListReservedWidthMm
      val areaY            = contentTopY
      val areaW            = availableW - co.colorListReservedWidthMm
      val areaH            = availableH - co.contentTopOffsetFromTopMm - contentTopOffsetMm
      val rightAreaW       = areaW * co.explodedAreaScaleFactor
      val rightAreaH       = areaH * co.explodedAreaScaleFactor
      val rightAreaX       = areaX + areaW - rightAreaW
      val nColsStepSection = sectionPic.width / stepSizePx
      val nRowsStepSection = sectionPic.height / stepSizePx
      val stepGridForSection =
        if (nColsStepSection >= 1 && nRowsStepSection >= 1) {
          val stepParts = (0 until nRowsStepSection).flatMap { cy =>
            (0 until nColsStepSection).map { cx =>
              GridPart(cx * stepSizePx, cy * stepSizePx, stepSizePx, stepSizePx)
            }
          }
          Layout(nColsStepSection, nRowsStepSection, stepParts.toArray)
        } else {
          Layout(1, 1, Array(GridPart(0, 0, sectionPic.width, sectionPic.height)))
        }
      val explodedInstrs = explodedOverviewInstructions(
        sectionPic,
        stepGridForSection,
        rightAreaX,
        areaY,
        rightAreaW,
        rightAreaH,
        co.explodedGapMm,
        co.explodedDimensionGapMm,
        co.explodedDimensionFontSizePt,
        co.explodedDimensionLineWidthMm,
        "right"
      )
      val allSectionsDivisible = parts.forall(p => p.width % stepSizePx == 0 && p.height % stepSizePx == 0)
      val divisibilityNote =
        if (allSectionsDivisible) Nil
        else {
          val noteY = gridOverviewY + smallOverviewH + 4.0
          List(
            Instruction.FontSize(co.divisibilityNoteFontSizePt),
            Instruction.Text(
              marginLR,
              noteY,
              s"Note: some sections don't match the step size ($stepSizePx); only matching sections get step-by-step pages.")
          )
        }
      val chapterOverviewPage =
        List(Instruction.AddPage) ++ colorCountInstrs ++ smallOverviewInstrs ++ divisibilityNote ++ explodedInstrs

      val thisSectionDivisible = sectionPic.width % stepSizePx == 0 && sectionPic.height % stepSizePx == 0
      val sectionInstrs = if (!thisSectionDivisible) {
        val msg =
          s"Section $sectionIndex: size (${sectionPic.width}×${sectionPic.height}) doesn't work with the step size ($stepSizePx). Step-by-step pages are skipped for it."
        List(
          Instruction.AddPage,
          Instruction.FontSize(co.nonDivisibleMessageFontSizePt),
          Instruction.Text(marginLR, marginTB + co.nonDivisibleMessageOffsetFromTopMm, msg)
        )
      } else {
        sectionInstructionsForSection(
          sectionPic,
          fullPic,
          grid,
          part,
          stepSizePx,
          marginLR,
          marginTB,
          availableW,
          availableH,
          contentTopOffsetMm,
          patchBgColor,
          stacked,
          config)
      }
      chapterOverviewPage ++ sectionInstrs
    }
  }

  /** One section per step: layered canvases (4 per page) for each step patch. */
  private def sectionInstructionsForSection(
    sectionPic: PixelPic,
    fullPic: PixelPic,
    grid: Layout,
    part: GridPart,
    stepSizePx: Int,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    availableH: Double,
    contentTopOffsetMm: Double,
    patchBgColor: Color,
    stacked: Boolean,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val nCols = sectionPic.width / stepSizePx
    val nRows = sectionPic.height / stepSizePx
    (0 until nRows)
      .flatMap { cy =>
        (0 until nCols).map { cx =>
          val stepX = cx * stepSizePx
          val stepY = cy * stepSizePx
          sectionPic.crop(stepX, stepY, stepSizePx, stepSizePx) match {
            case Some(patch) =>
              layerPagesForPatch(
                patch,
                fullPic,
                stepSizePx,
                cx,
                cy,
                part,
                grid,
                marginLR,
                marginTB,
                availableW,
                availableH,
                contentTopOffsetMm,
                patchBgColor,
                stacked,
                config)
            case None =>
              Nil
          }
        }
      }
      .toList
      .flatten
  }

  /** Left column for a step page: patch color counts, small full-mosaic overview with non-current sections and other
    * steps greyed (only current step in full color).
    */
  private def stepPageLeftColumnInstructions(
    fullPic: PixelPic,
    grid: Layout,
    part: GridPart,
    patch: PixelPic,
    stepCx: Int,
    stepCy: Int,
    stepSizePx: Int,
    marginLR: Double,
    marginTB: Double,
    contentTopOffsetMm: Double,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val co          = config.chapterOverview
    val sw          = co.swatch
    val parts       = grid.parts
    val contentTopY = marginTB + co.contentTopOffsetFromTopMm + contentTopOffsetMm
    val colorRows = patch.palette.toVector.sortBy(-_._2).map { case (idx, count) =>
      val px = fullPic.paletteLookup(idx)
      (px.r, px.g, px.b, count)
    }
    val colorCountInstrs = PdfDrawHelpers.drawSwatchRows(marginLR, contentTopY, colorRows, sw)
    val colorListBottom  = contentTopY + sw.firstLineOffsetMm + colorRows.size * sw.lineHeightMm
    val gridOverviewY    = colorListBottom + co.gridOverviewGapAboveMm
    val (smallX0, smallY0, smallScale, smallOverviewW, smallOverviewH) =
      PdfDrawHelpers.smallOverviewLayoutParams(fullPic, gridOverviewY, marginLR, co)
    val greyRectsOtherSections = parts.toList
      .filter(_ != part)
      .map(p =>
        (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
    val nColsSection = part.width / stepSizePx
    val nRowsSection = part.height / stepSizePx
    val greyRectsOtherSteps = (0 until nRowsSection).flatMap { cy =>
      (0 until nColsSection).filter(cx => cx != stepCx || cy != stepCy).map { cx =>
        val gx = part.x + cx * stepSizePx
        val gy = part.y + cy * stepSizePx
        (smallX0 + gx * smallScale, smallY0 + gy * smallScale, stepSizePx * smallScale, stepSizePx * smallScale)
      }
    }.toList
    val greyRectsMm = greyRectsOtherSections ++ greyRectsOtherSteps
    val stepGlobalX = part.x + stepCx * stepSizePx
    val stepGlobalY = part.y + stepCy * stepSizePx
    val highlightedRectMm = (
      smallX0 + stepGlobalX * smallScale,
      smallY0 + stepGlobalY * smallScale,
      stepSizePx * smallScale,
      stepSizePx * smallScale)
    val smallOverviewInstrs =
      PdfDrawHelpers.drawSmallOverview(
        fullPic,
        smallX0,
        smallY0,
        smallOverviewW,
        smallOverviewH,
        greyRectsMm,
        highlightedRectMm,
        grid)
    colorCountInstrs ++ smallOverviewInstrs
  }

  /** Layered canvases for one step's patch: cumulative colors (least→most), 4 per page, 16×16 and 4×4 grids. Left
    * column = patch counts + small overview + step marker.
    */
  private def layerPagesForPatch(
    patch: PixelPic,
    fullPic: PixelPic,
    stepSizePx: Int,
    stepCx: Int,
    stepCy: Int,
    part: GridPart,
    grid: Layout,
    marginLR: Double,
    marginTB: Double,
    availableW: Double,
    availableH: Double,
    contentTopOffsetMm: Double,
    patchBgColor: Color,
    stacked: Boolean,
    config: PdfLayoutConfig
  ): List[Instruction] = {
    val lp             = config.layerPage
    val co             = config.chapterOverview
    val contentTopY    = marginTB + co.contentTopOffsetFromTopMm + contentTopOffsetMm
    val rightAreaX     = marginLR + co.colorListReservedWidthMm
    val rightAreaW     = availableW - co.colorListReservedWidthMm
    val rightAreaY     = contentTopY
    val rightAreaH     = availableH - co.contentTopOffsetFromTopMm - contentTopOffsetMm
    val layerGridScale = 0.85
    val effectiveW     = rightAreaW * layerGridScale
    val effectiveH     = rightAreaH * layerGridScale
    val patchRows      = (lp.patchesPerPage + lp.patchGridCols - 1) / lp.patchGridCols
    val patchCellMm = (
      (effectiveW - (lp.patchGridCols - 1) * lp.patchGapMm) / lp.patchGridCols
    ).min((effectiveH - (patchRows - 1) * lp.patchGapMm) / patchRows)
    val totalGridW      = lp.patchGridCols * patchCellMm + (lp.patchGridCols - 1) * lp.patchGapMm
    val startX          = rightAreaX + rightAreaW - totalGridW
    val startY          = rightAreaY
    val bgRgb = (patchBgColor.r, patchBgColor.g, patchBgColor.b)
    val colorIndicesAsc = patch.getIndexByCount
    val (_, layerRgbFlats) = if (stacked) {
      colorIndicesAsc.foldLeft((Set.empty[Int], Vector.empty[Vector[Int]])) {
        case ((set, acc), idx) =>
          val newSet = set + idx
          (newSet, acc :+ PdfDrawHelpers.buildLayerRgb(patch, newSet, (i: Int) => fullPic.paletteLookup(i), bgRgb))
      }
    } else {
      colorIndicesAsc.foldLeft((Set.empty[Int], Vector.empty[Vector[Int]])) {
        case ((set, acc), idx) =>
          val newSet = set + idx
          (newSet, acc :+ PdfDrawHelpers.buildLayerRgb(patch, Set(idx), (i: Int) => fullPic.paletteLookup(i), bgRgb))
      }
    }
    val layerRgbFlatsList = layerRgbFlats.toList
    val patchW            = patch.width
    val patchH            = patch.height
    val leftColumnInstrs =
      stepPageLeftColumnInstructions(
        fullPic,
        grid,
        part,
        patch,
        stepCx,
        stepCy,
        stepSizePx,
        marginLR,
        marginTB,
        contentTopOffsetMm,
        config)
    layerRgbFlatsList.grouped(lp.patchesPerPage).toList.zipWithIndex.flatMap { case (layerBatch, batchIdx) =>
      val scale  = (patchCellMm / patchW).min(patchCellMm / patchH)
      val imageW = patchW * scale
      val imageH = patchH * scale
      val perPage = layerBatch.zipWithIndex.map { case (rgbFlat, i) =>
        val col   = i % lp.patchGridCols
        val row   = i / lp.patchGridCols
        val cellX = startX + col * (patchCellMm + lp.patchGapMm)
        val cellY = startY + row * (patchCellMm + lp.patchGapMm)
        val x0    = cellX + (patchCellMm - imageW) / 2
        val y0    = cellY + (patchCellMm - imageH) / 2
        (x0, y0, imageW, imageH, rgbFlat)
      }
      val patchBorderLineWidthMm = 0.35
      List(Instruction.AddPage) ++ leftColumnInstrs ++ perPage.flatMap { case (x0, y0, w, h, rgb) =>
        val grid16Rects =
          PdfDrawHelpers.grid16x16PatchStrokeRects(x0, y0, w, h, patchW, patchH, stepSizePx, lp.grid16LineWidthMm)
        val grid4x4Rects  = PdfDrawHelpers.grid4x4StrokeRects(x0, y0, w, h, lp.grid4x4LineWidthMm)
        val grid16Dashed  = PdfDrawHelpers.dashedStrokeRectsInstructions(grid16Rects, 0.9, 1.0)
        val grid4x4Dashed = PdfDrawHelpers.dashedStrokeRectsInstructions(grid4x4Rects, 0.9, 1.0)
        val borderInstr = Instruction.DrawStrokeRects(
          List((x0, y0, w, h)),
          Color.black.r,
          Color.black.g,
          Color.black.b,
          patchBorderLineWidthMm)
        List(
          Instruction.DrawPixelGrid(x0, y0, w, h, patchW, patchH, rgb)) ++ grid16Dashed ++ grid4x4Dashed :+ borderInstr
      }
    }
  }
}
