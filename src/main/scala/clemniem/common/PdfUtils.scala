package clemniem.common

import cats.effect.IO
import clemniem.{Color, GridConfig, GridPart, Pixel, PixelPic}
import clemniem.common.pdf.{Instruction, JsPDF, PdfLayout, PdfLayoutConfig}

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
  val defaultPageBackgroundColor: Color = Color(253, 251, 230)

  /** Background RGB for layer patches (pixels not in the current cumulative set). Later configurable. */
  private val layerPatchBackgroundGrey = 220

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
        val fullOverview = fullOverviewPageInstructions(pic, marginLR, marginTB, availableW, availableH, config)
        val chapters     = allChaptersInstructions(pic, grid, marginLR, marginTB, availableW, availableH, stepSizePx, config)
        (cover, emptyPage ++ fullOverview, chapters)
      case None =>
        (PdfLayout.coverInstructions(title, printerMarginMm, config), List(Instruction.AddPage), Nil)
    }
    val instructions = coverInstrs ++ afterCoverInstrs ++ chapterInstrs :+ Instruction.Save("mosaic-book.pdf")
    JsPDF.run(instructions, pageBackgroundColor.r, pageBackgroundColor.g, pageBackgroundColor.b, printerMarginMm)
  }

  /** Cover page: title above full mosaic, no grids. */
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
    val c               = config.cover
    val titleBlockHeight = c.titleBlockHeightMm
    val titleY           = marginTB + c.titleOffsetYFromTopMm
    val mosaicAvailableH = pageH - marginTB - titleBlockHeight - marginTB
    val (pw, ph, rgbFlat) = pixelPicToRgbFlat(pic)
    val scale             = (availableW / pw).min(mosaicAvailableH / ph)
    val imageW            = pw * scale
    val imageH            = ph * scale
    val x0                = marginLR + (availableW - imageW) / 2
    val y0                = marginTB + titleBlockHeight + (mosaicAvailableH - imageH) / 2
    List(
      Instruction.PageSize(pageW, pageH),
      Instruction.FontSize(c.titleFontSizePt),
      Instruction.Text(marginLR, titleY, title),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat)
    )
  }

  /** One page after the empty page: full-size mosaic overview and all colors for the whole mosaic. */
  private def fullOverviewPageInstructions(
      fullPic: PixelPic,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val fo  = config.fullOverview
    val sw  = fo.swatch
    val (pw, ph, rgbFlat) = pixelPicToRgbFlat(fullPic)
    val mosaicAvailableH   = availableH - fo.titleBlockHeightMm - fo.colorListReservedHeightMm
    val scale              = (availableW / pw).min(mosaicAvailableH / ph)
    val imageW             = pw * scale
    val imageH             = ph * scale
    val x0                 = marginLR + (availableW - imageW) / 2
    val y0                 = marginTB + fo.titleBlockHeightMm + (mosaicAvailableH - imageH) / 2
    val countYStart        = y0 + imageH + fo.countListGapBelowImageMm
    val colorCountInstrs   = fullPic.palette.toVector.sortBy(-_._2).zipWithIndex.flatMap { case ((idx, count), i) =>
      val px = fullPic.paletteLookup(idx)
      val y  = countYStart + sw.firstLineOffsetMm + i * sw.lineHeightMm
      val x  = marginLR
      List(
        Instruction.FillRect(x, y, sw.swatchSizeMm, sw.swatchSizeMm, px.r, px.g, px.b),
        Instruction.FontSize(sw.countFontSizePt),
        Instruction.Text(x + sw.swatchSizeMm + sw.swatchGapMm, y + sw.swatchSizeMm * sw.swatchTextVerticalRatio, s"× $count")
      )
    }.toList
    List(
      Instruction.AddPage,
      Instruction.FontSize(fo.titleFontSizePt),
      Instruction.Text(marginLR, marginTB + fo.titleOffsetFromTopMm, "Full mosaic – all colors"),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat),
      Instruction.FontSize(fo.countLabelFontSizePt),
      Instruction.Text(marginLR, countYStart, "Colors for whole mosaic:")
    ) ++ colorCountInstrs
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

  /** One chapter (one plate): overview page + sections (one per build step). Each section: overview showing step in plate, then layered canvases (least→most). */
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
    val parts        = grid.parts
    val platePicOpt  = fullPic.crop(part.x, part.y, part.width, part.height)
    if (platePicOpt.isEmpty) Nil
    else {
      val platePic = platePicOpt.get
      val plateRgb         = pixelPicToRgbFlat(platePic)
      val plateScale       = (availableW / platePic.width).min(availableH / (platePic.height + co.plateImageHeightReserveForScalePx))
      val plateImageH      = platePic.height * plateScale
      val plateImageW      = platePic.width * plateScale
      val plateX0          = marginLR + (availableW - plateImageW) / 2
      val plateY0          = marginTB + co.plateImageTopOffsetMm
      val countYStart      = plateY0 + plateImageH + co.countListGapBelowImageMm
      val colorCountInstrs = platePic.palette.toVector.sortBy(-_._2).zipWithIndex.flatMap { case ((idx, count), i) =>
        val px = fullPic.paletteLookup(idx)
        val y  = countYStart + sw.firstLineOffsetMm + i * sw.lineHeightMm
        val x  = marginLR
        List(
          Instruction.FillRect(x, y, sw.swatchSizeMm, sw.swatchSizeMm, px.r, px.g, px.b),
          Instruction.FontSize(sw.countFontSizePt),
          Instruction.Text(x + sw.swatchSizeMm + sw.swatchGapMm, y + sw.swatchSizeMm * sw.swatchTextVerticalRatio, s"× $count")
        )
      }.toList
      val smallOverviewH = co.smallOverviewHeightMm
      val smallOverviewW = fullPic.width * (smallOverviewH / fullPic.height)
      val smallX0        = marginLR + (availableW - smallOverviewW) / 2
      val smallY0        = marginTB + co.smallOverviewTopOffsetMm
      val smallScale     = smallOverviewW / fullPic.width
      val smallGridRects = parts.toList.map(p => (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
      val smallCurrent   = List((smallX0 + part.x * smallScale, smallY0 + part.y * smallScale, part.width * smallScale, part.height * smallScale))
      val allPlatesDivisible = parts.forall(p => p.width % stepSizePx == 0 && p.height % stepSizePx == 0)
      val divisibilityNote = if (allPlatesDivisible) Nil else {
        List(
          Instruction.FontSize(co.divisibilityNoteFontSizePt),
          Instruction.Text(marginLR, countYStart - co.divisibilityNoteOffsetAboveCountYMm, s"Note: not all plates divisible by step size $stepSizePx; only divisible plates have step sections.")
        )
      }
      val chapterOverviewPage = List(
        Instruction.AddPage,
        Instruction.FontSize(co.chapterTitleFontSizePt),
        Instruction.Text(marginLR, marginTB + co.chapterTitleOffsetFromTopMm, s"Chapter $plateIndex – Plate $plateIndex"),
        Instruction.DrawPixelGrid(smallX0, smallY0, smallOverviewW, smallOverviewH, fullPic.width, fullPic.height, pixelPicToRgbFlat(fullPic)._3),
        Instruction.DrawStrokeRects(smallGridRects, 255, 0, 0),
        Instruction.DrawStrokeRects(smallCurrent, 0, 0, 255),
        Instruction.DrawPixelGrid(plateX0, plateY0, plateImageW, plateImageH, platePic.width, platePic.height, plateRgb._3),
        Instruction.FontSize(co.countLabelFontSizePt),
        Instruction.Text(marginLR, countYStart, "Colors for this plate:")
      ) ++ divisibilityNote ++ colorCountInstrs

      val thisPlateDivisible = platePic.width % stepSizePx == 0 && platePic.height % stepSizePx == 0
      val sectionInstrs = if (!thisPlateDivisible) {
        val msg = s"Plate $plateIndex dimensions (${platePic.width}×${platePic.height}) are not divisible by step size $stepSizePx; step-by-step sections omitted."
        List(Instruction.AddPage, Instruction.FontSize(co.nonDivisibleMessageFontSizePt), Instruction.Text(marginLR, marginTB + co.nonDivisibleMessageOffsetFromTopMm, msg))
      } else {
        sectionInstructionsForPlate(platePic, fullPic, stepSizePx, plateIndex, marginLR, marginTB, plateX0, plateY0, plateImageW, plateImageH, config)
      }
      chapterOverviewPage ++ sectionInstrs
    }
  }

  /** One section per step: section overview (plate with step highlighted) + layered canvases (4 per page). */
  private def sectionInstructionsForPlate(
      platePic: PixelPic,
      fullPic: PixelPic,
      stepSizePx: Int,
      plateIndex: Int,
      marginLR: Double,
      marginTB: Double,
      plateX0: Double,
      plateY0: Double,
      plateImageW: Double,
      plateImageH: Double,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val plateRgb     = pixelPicToRgbFlat(platePic)
    val nCols        = platePic.width / stepSizePx
    val nRows        = platePic.height / stepSizePx
    (0 until nRows).flatMap { cy =>
      (0 until nCols).map { cx =>
        val stepNum = cy * nCols + cx + 1
        val stepX   = cx * stepSizePx
        val stepY   = cy * stepSizePx
        platePic.crop(stepX, stepY, stepSizePx, stepSizePx) match {
          case Some(patch) =>
            val sectionOverviewPage = sectionOverviewInstructions(
              stepNum, platePic, plateRgb, plateIndex, marginLR, marginTB, plateX0, plateY0, plateImageW, plateImageH, cx, cy, stepSizePx, config
            )
            val layerInstrs = layerPagesForPatch(patch, fullPic, stepSizePx, plateIndex, stepNum, config)
            sectionOverviewPage ++ layerInstrs
          case None =>
            Nil
        }
      }
    }.toList.flatten
  }

  /** Section overview: full plate with step region highlighted. */
  private def sectionOverviewInstructions(
      stepNum: Int,
      platePic: PixelPic,
      plateRgb: (Int, Int, Vector[Int]),
      plateIndex: Int,
      marginLR: Double,
      marginTB: Double,
      plateX0: Double,
      plateY0: Double,
      plateImageW: Double,
      plateImageH: Double,
      cx: Int,
      cy: Int,
      stepSizePx: Int,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val so = config.sectionOverview
    val stepRectMm = {
      val x = plateX0 + cx * stepSizePx * (plateImageW / platePic.width)
      val y = plateY0 + cy * stepSizePx * (plateImageH / platePic.height)
      val w = stepSizePx * (plateImageW / platePic.width)
      val h = stepSizePx * (plateImageH / platePic.height)
      (x, y, w, h)
    }
    List(
      Instruction.AddPage,
      Instruction.FontSize(so.titleFontSizePt),
      Instruction.Text(marginLR, marginTB + so.titleOffsetFromTopMm, s"Plate $plateIndex – Step $stepNum (section overview)"),
      Instruction.FontSize(so.subtitleFontSizePt),
      Instruction.Text(marginLR, marginTB + so.subtitleOffsetFromTopMm, "All colors – step region highlighted"),
      Instruction.DrawPixelGrid(plateX0, plateY0, plateImageW, plateImageH, plateRgb._1, plateRgb._2, plateRgb._3),
      Instruction.DrawStrokeRects(List(stepRectMm), 0, 180, 0, so.stepHighlightLineWidthMm)
    )
  }

  /** Layered canvases for one step's patch: cumulative colors (least→most), 4 per page, 16×16 and 4×4 grids. */
  private def layerPagesForPatch(
      patch: PixelPic,
      fullPic: PixelPic,
      stepSizePx: Int,
      plateIndex: Int,
      stepNum: Int,
      config: PdfLayoutConfig
  ): List[Instruction] = {
    val lp          = config.layerPage
    val patchCellMm = (config.global.pageSizeMm - 2 * lp.patchMarginMm - lp.patchGapMm) / lp.patchGridCols
    val colorIndicesAsc = patch.getIndexByCount
    val (_, layerRgbFlats) = colorIndicesAsc.foldLeft((Set.empty[Int], Vector.empty[Vector[Int]])) {
      case ((set, acc), idx) =>
        val newSet = set + idx
        (newSet, acc :+ buildCumulativeLayerRgb(patch, newSet, (i: Int) => fullPic.paletteLookup(i)))
    }
    val layerRgbFlatsList = layerRgbFlats.toList
    val patchW = patch.width
    val patchH = patch.height
    layerRgbFlatsList.grouped(lp.patchesPerPage).toList.zipWithIndex.flatMap { case (layerBatch, batchIdx) =>
      val startX = lp.patchMarginMm
      val startY = lp.patchMarginMm + lp.contentTopOffsetMm
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
        val labelY  = cellY + patchCellMm + lp.labelOffsetBelowPatchMm
        val layerNum = batchIdx * lp.patchesPerPage + i + 1
        (x0, y0, imageW, imageH, rgbFlat, labelY, layerNum)
      }
      List(
        Instruction.AddPage,
        Instruction.FontSize(lp.titleFontSizePt),
        Instruction.Text(lp.patchMarginMm, lp.patchMarginMm + lp.titleOffsetFromTopMm, s"Plate $plateIndex – Step $stepNum – color layers (least → most)")
      ) ++ perPage.flatMap { case (x0, y0, w, h, rgb, labelY, layerNum) =>
        val grid16Rects = grid16x16PatchStrokeRects(x0, y0, w, h, patchW, patchH, stepSizePx, lp.grid16LineWidthMm)
        val grid4x4Rects = grid4x4StrokeRects(x0, y0, w, h, lp.grid4x4LineWidthMm)
        List(
          Instruction.DrawPixelGrid(x0, y0, w, h, patchW, patchH, rgb),
          Instruction.DrawStrokeRects(grid16Rects, lp.gridStrokeGrey, lp.gridStrokeGrey, lp.gridStrokeGrey, lp.grid16LineWidthMm),
          Instruction.DrawStrokeRects(grid4x4Rects, lp.gridStrokeGrey, lp.gridStrokeGrey, lp.gridStrokeGrey, lp.grid4x4LineWidthMm),
          Instruction.FontSize(lp.labelFontSizePt),
          Instruction.Text(x0, labelY, s"Layer $layerNum")
        )
      }
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
        } else (layerPatchBackgroundGrey, layerPatchBackgroundGrey, layerPatchBackgroundGrey)
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
