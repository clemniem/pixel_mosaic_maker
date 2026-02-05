package clemniem.common

import cats.effect.IO
import clemniem.{GridConfig, Pixel, PixelPic}
import clemniem.common.pdf.{Instruction, JsPDF, PdfLayout}

/** Request for the book PDF: title and optional mosaic (pic + grid). Both Print buttons use this. */
final case class PrintBookRequest(
    title: String,
    mosaicPicAndGridOpt: Option[(PixelPic, GridConfig)]
)

/** High-level PDF helpers. Build [[Instruction]]s and run them via [[JsPDF]]. */
object PdfUtils {

  /** Background RGB for layer patches (pixels not in the current cumulative set). Later configurable. */
  private val layerPatchBackgroundGrey = 220

  /** Generate a single test page: 20×20 cm, centered text "TEST", then trigger save. */
  def printTestPdf(): IO[Unit] = IO {
    val instructions: List[Instruction] = List(
      Instruction.PageSize(200, 200),
      Instruction.FontSize(40),
      Instruction.Text(85, 100, "TEST"),
      Instruction.Save("test.pdf")
    )
    JsPDF.run(instructions)
  }

  /** Two-page test for checkpoint: Cover + "Page 2" (A4). Use Print button to confirm multi-page works. */
  def printTwoPageTestPdf(): IO[Unit] = IO {
    val a4W = 210.0
    val a4H = 297.0
    val instructions: List[Instruction] = List(
      Instruction.PageSize(a4W, a4H),
      Instruction.FontSize(24),
      Instruction.Text(80, 140, "Cover"),
      Instruction.AddPage,
      Instruction.FontSize(16),
      Instruction.Text(20, 30, "Page 2"),
      Instruction.Save("mosaic-two-page-test.pdf")
    )
    JsPDF.run(instructions)
  }

  /** Generate the book PDF. Single entry point for both Print PDF buttons; pass a [[PrintBookRequest]]. */
  def printBookPdf(request: PrintBookRequest): IO[Unit] = IO {
    runPrintBookPdf(request.title, request.mosaicPicAndGridOpt)
  }

  private def runPrintBookPdf(title: String, mosaicPicAndGridOpt: Option[(PixelPic, GridConfig)]): Unit = {
    val (pageW, pageH) = (PdfLayout.pageSizeMm, PdfLayout.pageSizeMm) // 20×20 cm
    val marginLR       = 15.0
    val marginTB       = 15.0
    val availableW     = pageW - 2 * marginLR
    val availableH     = pageH - 2 * marginTB

    val (coverInstrs, chapterInstrs) = mosaicPicAndGridOpt match {
      case Some((pic, grid)) =>
        val cover  = coverWithMosaic(title, pic, pageW, pageH, marginLR, marginTB, availableW)
        val chapter1 = firstChapterInstructions(pic, grid, marginLR, marginTB, availableW, availableH)
        (cover, chapter1)
      case None =>
        (PdfLayout.coverInstructions(title), Nil)
    }
    val instructions = coverInstrs ++ chapterInstrs :+ Instruction.Save("mosaic-book.pdf")
    JsPDF.run(instructions)
  }

  /** Cover page: title above full mosaic, no grids. */
  private def coverWithMosaic(
      title: String,
      pic: PixelPic,
      pageW: Double,
      pageH: Double,
      marginLR: Double,
      marginTB: Double,
      availableW: Double
  ): List[Instruction] = {
    val titleBlockHeight = 12.0
    val titleY           = marginTB + 8.0
    val mosaicAvailableH = pageH - marginTB - titleBlockHeight - marginTB
    val (pw, ph, rgbFlat) = pixelPicToRgbFlat(pic)
    val scale             = (availableW / pw).min(mosaicAvailableH / ph)
    val imageW            = pw * scale
    val imageH            = ph * scale
    val x0                = marginLR + (availableW - imageW) / 2
    val y0                = marginTB + titleBlockHeight + (mosaicAvailableH - imageH) / 2
    List(
      Instruction.PageSize(pageW, pageH),
      Instruction.FontSize(PdfLayout.coverTitleFontSize),
      Instruction.Text(marginLR, titleY, title),
      Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat)
    )
  }

  /** First chapter (plate 0): plate overview page + single-color patch pages (up to 4 per page). */
  private def firstChapterInstructions(
      fullPic: PixelPic,
      grid: GridConfig,
      marginLR: Double,
      marginTB: Double,
      availableW: Double,
      availableH: Double
  ): List[Instruction] = {
    val parts = grid.parts
    if (parts.isEmpty) Nil
    else {
      val part         = parts(0)
      val platePicOpt  = fullPic.crop(part.x, part.y, part.width, part.height)
      if (platePicOpt.isEmpty) Nil
      else {
        val platePic = platePicOpt.get
        val plateRgb         = pixelPicToRgbFlat(platePic)
        val plateScale       = (availableW / platePic.width).min(availableH / (platePic.height + 30))
        val plateImageH      = platePic.height * plateScale
        val plateImageW      = platePic.width * plateScale
        val plateX0     = marginLR + (availableW - plateImageW) / 2
        val plateY0     = marginTB + 55
        val countYStart = plateY0 + plateImageH + 6
        val swatchSize  = 4.0
        val swatchGap   = 1.0
        val lineHeight  = 5.0
        val colorCountInstrs = platePic.palette.toVector.sortBy(-_._2).zipWithIndex.flatMap { case ((idx, count), i) =>
          val px = fullPic.paletteLookup(idx)
          val y  = countYStart + 5 + i * lineHeight
          val x  = marginLR
          List(
            Instruction.FillRect(x, y, swatchSize, swatchSize, px.r, px.g, px.b),
            Instruction.FontSize(10),
            Instruction.Text(x + swatchSize + swatchGap, y + swatchSize * 0.75, s"× $count")
          )
        }.toList
        val smallOverviewH = 45.0
        val smallOverviewW = fullPic.width * (smallOverviewH / fullPic.height)
        val smallX0        = marginLR + (availableW - smallOverviewW) / 2
        val smallY0        = marginTB + 6
        val smallScale     = smallOverviewW / fullPic.width
        val smallGridRects = parts.toList.map(p => (smallX0 + p.x * smallScale, smallY0 + p.y * smallScale, p.width * smallScale, p.height * smallScale))
        val smallCurrent   = List((smallX0 + part.x * smallScale, smallY0 + part.y * smallScale, part.width * smallScale, part.height * smallScale))
        val chapterOverviewPage = List(
          Instruction.AddPage,
          Instruction.FontSize(12),
          Instruction.Text(marginLR, marginTB + 4, "Chapter 1 – Plate 1"),
          Instruction.DrawPixelGrid(smallX0, smallY0, smallOverviewW, smallOverviewH, fullPic.width, fullPic.height, pixelPicToRgbFlat(fullPic)._3),
          Instruction.DrawStrokeRects(smallGridRects, 255, 0, 0),
          Instruction.DrawStrokeRects(smallCurrent, 0, 0, 255),
          Instruction.DrawPixelGrid(plateX0, plateY0, plateImageW, plateImageH, platePic.width, platePic.height, plateRgb._3),
          Instruction.FontSize(10),
          Instruction.Text(marginLR, countYStart, "Colors for this plate:")
        ) ++ colorCountInstrs

        // Layer patches: same drawing as BuildScreen preview — full pixel resolution, cumulative colors (least→most), one layer = one "panel".
        val patchSizePx     = 16
        val colorIndicesAsc = platePic.getIndexByCount
        val layerRgbFlats   = (0 until colorIndicesAsc.length).map { k =>
          buildCumulativeLayerRgb(platePic, colorIndicesAsc.take(k + 1).toSet, (i: Int) => fullPic.paletteLookup(i))
        }.toList
        val patchMargin     = 15.0
        val patchGap        = 5.0
        val patchCellMm     = (availableW + 2 * marginLR - 2 * patchMargin - patchGap) / 2  // 2×2 grid (same 20×20 cm page)
        val patchPages = layerRgbFlats.grouped(4).toList.zipWithIndex.flatMap { case (layerBatch, batchIdx) =>
          val startX = patchMargin
          val startY = patchMargin + 10  // title line
          val scale  = (patchCellMm / platePic.width).min(patchCellMm / platePic.height)
          val imageW = platePic.width * scale
          val imageH = platePic.height * scale
          val perPage = layerBatch.zipWithIndex.map { case (rgbFlat, i) =>
            val col = i % 2
            val row = i / 2
            val cellX = startX + col * (patchCellMm + patchGap)
            val cellY = startY + row * (patchCellMm + patchGap)
            val x0    = cellX + (patchCellMm - imageW) / 2
            val y0    = cellY + (patchCellMm - imageH) / 2
            val labelY = cellY + patchCellMm + 4
            val layerNum = batchIdx * 4 + i + 1
            (x0, y0, imageW, imageH, rgbFlat, labelY, layerNum)
          }
          List(
            Instruction.AddPage,
            Instruction.FontSize(10),
            Instruction.Text(patchMargin, patchMargin + 6, "Plate 1 – color layers (least → most)")
          ) ++ perPage.flatMap { case (x0, y0, w, h, rgb, labelY, layerNum) =>
            val grid16Rects = grid16x16PatchStrokeRects(x0, y0, w, h, platePic.width, platePic.height, patchSizePx, lineWidthMm = 0.25)
            val grid4x4Rects = grid4x4StrokeRects(x0, y0, w, h, lineWidthMm = 0.1)
            val gridGrey = 120
            List(
              Instruction.DrawPixelGrid(x0, y0, w, h, platePic.width, platePic.height, rgb),
              Instruction.DrawStrokeRects(grid16Rects, gridGrey, gridGrey, gridGrey, 0.25),
              Instruction.DrawStrokeRects(grid4x4Rects, gridGrey, gridGrey, gridGrey, 0.1),
              Instruction.FontSize(8),
              Instruction.Text(x0, labelY, s"Layer $layerNum")
            )
          }
        }
        chapterOverviewPage ++ patchPages
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
