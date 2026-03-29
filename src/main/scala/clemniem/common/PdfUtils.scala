package clemniem.common

import cats.effect.IO
import clemniem.{Color, Layout, PixelPic}
import clemniem.common.pdf.{Instruction, JsPDF, PdfLayout, PdfLayoutConfig, PdfPageBuilders}

/** Request for the book PDF: title, optional mosaic (pic + grid), step size, page background color, printer margin (mm;
  * stays white), and optional layout config.
  */
final case class PrintBookRequest(
  title: String,
  mosaicPicAndGridOpt: Option[(PixelPic, Layout)],
  stepSizePx: Int = PdfUtils.defaultStepSizePx,
  pageBackgroundColor: Color = PdfUtils.defaultPageBackgroundColor,
  printerMarginMm: Double = PdfUtils.defaultPrinterMarginMm,
  contentTopOffsetMm: Double = PdfUtils.defaultContentTopOffsetMm,
  patchBackgroundColor: Color = Color.layerPatchBackground,
  stacked: Boolean = PdfUtils.defaultStacked,
  innerMargin: Boolean = PdfUtils.defaultInnerMargin,
  layoutConfig: Option[PdfLayoutConfig] = None)

/** High-level PDF helpers. Build [[Instruction]]s and run them via [[JsPDF]]. */
object PdfUtils {

  /** Default page background: light pastel yellow (old LEGO-catalog style). */
  val defaultPageBackgroundColor: Color = Color.defaultPageBackground

  val defaultPrinterMarginMm: Double  = 6.0
  val defaultContentTopOffsetMm: Double = 2.0
  val defaultStepSizePx: Int          = 16
  val defaultTitle: String            = "Mosaic"
  val defaultStacked: Boolean         = true
  val defaultInnerMargin: Boolean     = false

  private val progressBarHeightMm = 3.0

  /** Progress bar at the bottom of the page, just above the printer margin (inside the colored background area).
    * Unfilled area is the page background (no separate track/shadow).
    *
    * Booklet rules:
    *   - No bar on the first 3 PDF pages (cover front, cover back, full overview) or the last page (back cover).
    *   - Pages N-1 and N-2 always show 100% — a fun flourish that ensures the bar is never empty near the end.
    *   - Progress is calculated over the actual building pages (4…N-2) only; the 5 fixed framing pages are excluded
    *     from the denominator so the bar starts near 0 on page 4 and reaches 100% naturally at page N-2.
    *   - Left pages fill during the first half of overall progress (0–50%).
    *   - Right pages fill during the second half (50–100%); left is fully filled once progress >= 50%.
    */
  private def progressBarInstructions(
    pageIndex1Based: Int,
    totalPages: Int,
    pageW: Double,
    pageH: Double,
    printerMarginMm: Double,
    removeInnerMargin: Boolean
  ): List[Instruction] = {
    val pageIndex0Based = pageIndex1Based - 1
    if (totalPages <= 0 || pageIndex0Based < 3 || pageIndex1Based == totalPages) Nil
    else {
      val m = printerMarginMm.max(0.0)
      val (barX, barW) = if (removeInnerMargin && m > 0) {
        val isRightHand = pageIndex0Based % 2 == 0
        val x = if (isRightHand) 0.0 else m
        (x, (pageW - m).max(0.0))
      } else {
        (m, (pageW - 2 * m).max(0.0))
      }
      val barY = pageH - m - progressBarHeightMm
      if (barY < 0) Nil
      else {
        val fillRatio =
          if (pageIndex1Based >= totalPages - 2) 1.0
          else {
            // Denominator = building pages only (4…N-2): excludes the 5 fixed framing pages.
            val contentPages    = (totalPages - 5).max(1)
            val overallProgress = (pageIndex1Based - 3).toDouble / contentPages
            val contentPageNum  = pageIndex0Based - 1
            val leftSide        = (contentPageNum % 2) == 0
            if (leftSide) math.min(1.0, overallProgress * 2)
            else math.max(0.0, (overallProgress - 0.5) * 2)
          }
        val fillW = (barW * fillRatio).max(0)
        if (fillW > 0)
          List(
            Instruction.FillRect(
              barX,
              barY,
              fillW,
              progressBarHeightMm,
              Color.progressBarBackgroundPastelBlue.r,
              Color.progressBarBackgroundPastelBlue.g,
              Color.progressBarBackgroundPastelBlue.b
            ))
        else Nil
      }
    }
  }

  /** Insert progress bar instructions before each AddPage (for the page we're leaving) and before Save (for the last
    * page). Uses foldLeft to avoid stack overflow on large documents.
    */
  private def insertProgressBars(
    instructions: List[Instruction],
    totalPages: Int,
    pageW: Double,
    pageH: Double,
    printerMarginMm: Double,
    removeInnerMargin: Boolean
  ): List[Instruction] = {
    type State = (Int, List[Instruction]) // currentPage, reversed result
    val (_, revResult) = instructions.foldLeft[State]((1, Nil)) { case ((currentPage, acc), inst) =>
      inst match {
        case Instruction.AddPage =>
          val bar = progressBarInstructions(currentPage, totalPages, pageW, pageH, printerMarginMm, removeInnerMargin)
          (currentPage + 1, Instruction.AddPage :: (bar.reverse ++ acc))
        case s @ Instruction.Save(_) =>
          val bar = progressBarInstructions(currentPage, totalPages, pageW, pageH, printerMarginMm, removeInnerMargin)
          (currentPage, s :: (bar.reverse ++ acc))
        case other =>
          (currentPage, other :: acc)
      }
    }
    revResult.reverse
  }

  /** Sanitize title for use as PDF filename: strip/replace invalid chars, fallback to "mosaic-book" if empty. */
  def filenameFromTitle(title: String): String = {
    val invalid = """[\\/:*?"<>|\n\r]+"""
    val sanitized = title.trim.replaceAll(invalid, "-").replaceAll("-+", "-").stripSuffix("-").trim
    if (sanitized.isEmpty) "mosaic-book" else sanitized
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
      request.contentTopOffsetMm,
      request.patchBackgroundColor,
      request.stacked,
      request.innerMargin,
      config
    )
  }

  final case class BookPreview(
    pageWmm: Double,
    pageHmm: Double,
    printerMarginMm: Double,
    removeInnerMargin: Boolean,
    totalPages: Int,
    pages: Vector[List[Instruction]])

  /** Build a small preview of the PDF as per-page [[Instruction]] lists (no Save). Intended for rendering onto a canvas
    * in the UI. Includes progress bars. Returns at most `maxPages` pages (from the start).
    */
  def previewBookPages(request: PrintBookRequest, maxPages: Int): BookPreview = {
    val config          = request.layoutConfig.getOrElse(PdfLayoutConfig.default)
    val (pageW, pageH)  = (config.global.pageSizeMm, config.global.pageSizeMm)
    val printerMarginMm = request.printerMarginMm
    val marginLR        = printerMarginMm + config.global.contentPaddingLRMm
    val marginTB        = printerMarginMm + config.global.contentPaddingTBMm
    val availableW      = pageW - 2 * marginLR
    val availableH      = pageH - 2 * marginTB

    val contentInstrs = request.mosaicPicAndGridOpt match {
      case Some((pic, grid)) =>
        val cover        = PdfPageBuilders.coverWithMosaic(request.title, pic, pageW, pageH, marginLR, marginTB, availableW, config)
        val fullOverview = PdfPageBuilders.fullOverviewPageInstructions(pic, grid, marginLR, marginTB, availableW, availableH, request.contentTopOffsetMm, config)
        val chapters     = PdfPageBuilders.allChaptersInstructions(pic, grid, marginLR, marginTB, availableW, availableH, request.stepSizePx, request.contentTopOffsetMm, request.patchBackgroundColor, request.stacked, config)
        val chapterPages = chapters.count { case Instruction.AddPage => true; case _ => false }
        val padding      = if (chapterPages % 2 == 0) PdfPageBuilders.fullOverviewPageInstructions(pic, grid, marginLR, marginTB, availableW, availableH, request.contentTopOffsetMm, config) else Nil
        val backCover    = Instruction.AddPage :: PdfPageBuilders.coverMosaicImageOnly(pic, pageH, marginLR, marginTB, availableW, config)
        cover ++ List(Instruction.AddPage) ++ fullOverview ++ chapters ++ padding ++ List(Instruction.AddPage) ++ backCover
      case None =>
        PdfLayout.coverInstructions(request.title, printerMarginMm, config) ++ List(Instruction.AddPage)
    }

    val removeInner     = !request.innerMargin
    val rawInstructions = contentInstrs :+ Instruction.Save("__preview__.pdf")
    val totalPages      = 1 + rawInstructions.count { case Instruction.AddPage => true; case _ => false }
    val withBars        = insertProgressBars(rawInstructions, totalPages, pageW, pageH, printerMarginMm, removeInner)
    val noSave          = withBars.filterNot { case Instruction.Save(_) => true; case _ => false }
    val pages           = splitIntoPages(noSave).take(maxPages.max(1))
    BookPreview(pageW, pageH, printerMarginMm, removeInner, totalPages, pages)
  }

  /** Split a full instruction list into per-page lists (AddPage is the boundary). */
  private def splitIntoPages(instructions: List[Instruction]): Vector[List[Instruction]] = {
    type State = (Vector[List[Instruction]], List[Instruction]) // finished pages, current (reversed)
    val (pages, currentRev) = instructions.foldLeft[State]((Vector.empty, Nil)) { case ((done, curRev), inst) =>
      inst match {
        case Instruction.AddPage =>
          (done :+ curRev.reverse, Nil)
        case other =>
          (done, other :: curRev)
      }
    }
    pages :+ currentRev.reverse
  }

  /** Assemble and run the full book PDF. Page structure (two-sided print aware; total always even):
    *
    * | Page | Content                                                                    |
    * |------|----------------------------------------------------------------------------|
    * | 1    | Front cover (image + title)                                                |
    * | 2    | Empty (back of front cover)                                                |
    * | 3    | Full overview (exploded mosaic + color list)                                |
    * | 4…M  | Chapters (one chapter overview + step-by-step layer pages per section)     |
    * | M+1  | Extra full overview — only inserted when chapterPages is even, for parity   |
    * | N-1  | Empty (always)                                                             |
    * | N    | Back cover (image only, no title)                                          |
    *
    * Progress bar is drawn on pages 4…N-1 (skips first 3 pages and back cover). Pages N-1 and N-2 always show 100%.
    */
  private def runPrintBookPdf(
    title: String,
    mosaicPicAndGridOpt: Option[(PixelPic, Layout)],
    stepSizePx: Int,
    pageBackgroundColor: Color,
    printerMarginMm: Double,
    contentTopOffsetMm: Double,
    patchBgColor: Color,
    stacked: Boolean,
    innerMargin: Boolean,
    config: PdfLayoutConfig
  ): Unit = {
    val (pageW, pageH) = (config.global.pageSizeMm, config.global.pageSizeMm)
    val marginLR       = printerMarginMm + config.global.contentPaddingLRMm
    val marginTB       = printerMarginMm + config.global.contentPaddingTBMm
    val availableW     = pageW - 2 * marginLR
    val availableH     = pageH - 2 * marginTB

    val contentInstrs = mosaicPicAndGridOpt match {
      case Some((pic, grid)) =>
        val cover        = PdfPageBuilders.coverWithMosaic(title, pic, pageW, pageH, marginLR, marginTB, availableW, config)
        val fullOverview = PdfPageBuilders.fullOverviewPageInstructions(pic, grid, marginLR, marginTB, availableW, availableH, contentTopOffsetMm, config)
        val chapters     = PdfPageBuilders.allChaptersInstructions(pic, grid, marginLR, marginTB, availableW, availableH, stepSizePx, contentTopOffsetMm, patchBgColor, stacked, config)
        val chapterPages = chapters.count { case Instruction.AddPage => true; case _ => false }
        val padding      = if (chapterPages % 2 == 0) PdfPageBuilders.fullOverviewPageInstructions(pic, grid, marginLR, marginTB, availableW, availableH, contentTopOffsetMm, config) else Nil
        val backCover    = Instruction.AddPage :: PdfPageBuilders.coverMosaicImageOnly(pic, pageH, marginLR, marginTB, availableW, config)
        cover ++ List(Instruction.AddPage) ++ fullOverview ++ chapters ++ padding ++ List(Instruction.AddPage) ++ backCover
      case None =>
        PdfLayout.coverInstructions(title, printerMarginMm, config) ++ List(Instruction.AddPage)
    }
    val filename        = filenameFromTitle(title) + ".pdf"
    val rawInstructions = contentInstrs :+ Instruction.Save(filename)
    val totalPages      = 1 + rawInstructions.count { case Instruction.AddPage => true; case _ => false }
    val removeInner     = !innerMargin
    val instructions    = insertProgressBars(rawInstructions, totalPages, pageW, pageH, printerMarginMm, removeInner)
    val (bgR, bgG, bgB) = pageBackgroundColor.rgb
    JsPDF.run(instructions, bgR, bgG, bgB, printerMarginMm, removeInner)
  }
}
