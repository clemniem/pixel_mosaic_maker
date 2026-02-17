package clemniem.common.pdf

import clemniem.Color
import org.scalajs.dom.console
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

/** Runs PDF [[Instruction]]s using the jsPDF library (must be loaded on window.jspdf). All jsPDF API access is confined
  * to this object. Uses "Press Start 2P" from window.pressStartBase64 (fontData.js), same as gbcamutil.
  */
object JsPDF {

  /** VFS filename and font name: same as gbcamutil (PressStart2P.ttf, PressStart). */
  private val pressStart2PVfsName  = "PressStart2P.ttf"
  private val pressStart2PFontName = "PressStart"

  private def fillPageBackground(doc: js.Dynamic, w: Double, h: Double, r: Int, g: Int, b: Int, printerMarginMm: Double)
    : Unit = {
    val _ = doc.setFillColor(r, g, b)
    if (printerMarginMm <= 0) {
      val _ = doc.rect(0, 0, w, h, "F")
    } else {
      val x  = printerMarginMm
      val y  = printerMarginMm
      val rw = (w - 2 * printerMarginMm).max(0)
      val rh = (h - 2 * printerMarginMm).max(0)
      val _  = doc.rect(x, y, rw, rh, "F")
    }
  }

  /** Execute instructions after a short delay (so jsPDF global is ready). Uses window.pressStartBase64 from fontData.js
    * (same as gbcamutil).
    */
  def run(instructions: List[Instruction], bgR: Int, bgG: Int, bgB: Int, printerMarginMm: Double): Unit = {
    val base64Val = js.Dynamic.global.selectDynamic("pressStartBase64")
    val fontBase64Opt =
      if (js.typeOf(base64Val) == "undefined") None
      else Some(base64Val.asInstanceOf[String]).filter(_.nonEmpty)
    val _ = setTimeout(0)(runNow(instructions, bgR, bgG, bgB, printerMarginMm, fontBase64Opt))
  }

  /** Overload: pass background as [[Color]] (uses [[Color.rgb]]). */
  def run(instructions: List[Instruction], pageBackground: Color, printerMarginMm: Double): Unit = {
    val (r, g, b) = pageBackground.rgb
    run(instructions, r, g, b, printerMarginMm)
  }

  /** Total page count: 1 + number of AddPage (Save is not a page). */
  private def countPages(instructions: List[Instruction]): Int =
    1 + instructions.count { case Instruction.AddPage => true; case _ => false }

  /** Draw page number in outer corner for booklet: first 2 PDF pages (cover) and last 2 (cover back) have no number.
    * Open the cover: first right-hand page = content page 1, back of it = 2, etc. Even content page = left, odd =
    * right.
    */
  private def drawPageNumberIfNeeded(
    doc: js.Dynamic,
    pageW: Double,
    pageH: Double,
    pageIndex0Based: Int,
    totalPages: Int,
    printerMarginMm: Double
  ): Unit =
    if (pageIndex0Based >= 2 && pageIndex0Based <= totalPages - 3) {
      val contentPageNum = pageIndex0Based - 1 // first right-hand = 1, back of it = 2, ...
      val leftSide       = (contentPageNum % 2) == 0
      val _              = doc.setFontSize(9)
      val _              = doc.setTextColor(0, 0, 0)
      val y              = pageH - printerMarginMm - 5
      if (leftSide) {
        val x = printerMarginMm + 5 // 3mm inward from outer corner
        val _ = doc.text(contentPageNum.toString, x, y)
      } else {
        val x = pageW - printerMarginMm - 5 // 3mm inward from outer corner
        val _ = doc.text(contentPageNum.toString, x, y, js.Dynamic.literal(align = "right"))
      }
    }

  def runNow(
    instructions: List[Instruction],
    bgR: Int,
    bgG: Int,
    bgB: Int,
    printerMarginMm: Double,
    fontBase64Opt: Option[String]
  ): Unit =
    getJsPDFConstructor match {
      case None =>
        console.warn("JsPDF: jsPDF not found on window. Add script tag for jspdf.umd.min.js.")
      case Some(ctor) =>
        val totalPages = countPages(instructions)
        type State = (Option[js.Dynamic], (Double, Double), Int) // docOpt, (pageW, pageH), pageIndex (0-based)
        val _ = instructions.foldLeft[State]((Option.empty[js.Dynamic], (0.0, 0.0), 0)) { (state, inst) =>
          val (docOpt, (pageW, pageH), pageIndex) = state
          inst match {
            case Instruction.PageSize(w, h) =>
              val _ = docOpt
              val opts = js.Dynamic.literal(
                orientation = "p",
                unit = "mm",
                format = js.Array(w, h)
              )
              val doc = js.Dynamic.newInstance(ctor)(opts)
              fillPageBackground(doc, w, h, bgR, bgG, bgB, printerMarginMm)
              fontBase64Opt.foreach { base64 =>
                val _ = doc.addFileToVFS(pressStart2PVfsName, base64)
                val _ = doc.addFont(pressStart2PVfsName, pressStart2PFontName, "normal")
                val _ = doc.setFont(pressStart2PFontName)
              }
              (Some(doc), (w, h), 0)
            case Instruction.FontSize(pt) =>
              docOpt.foreach { doc =>
                val _ = doc.setFontSize(pt)
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.Text(x, y, value) =>
              docOpt.foreach { doc =>
                val _ = doc.text(value, x, y)
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.TextAligned(x, y, value, align, fontSizePt) =>
              docOpt.foreach { doc =>
                val _ = doc.setFontSize(fontSizePt)
                val _ = doc.setTextColor(0, 0, 0)
                val _ = doc.text(value, x, y, js.Dynamic.literal(align = align))
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.DrawLine(x1, y1, x2, y2, lineWidth, r, g, b) =>
              docOpt.foreach { doc =>
                val _ = doc.setDrawColor(r, g, b)
                val _ = doc.setLineWidth(lineWidth)
                val _ = doc.line(x1, y1, x2, y2)
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.TextWithBackground(x, yTop, value, fontSizePt, padding, alignLeft, bgR, bgG, bgB) =>
              docOpt.foreach { doc =>
                val _     = doc.setFontSize(fontSizePt)
                val dims  = doc.getTextDimensions(value).asInstanceOf[js.Dynamic]
                val tw    = dims.w.asInstanceOf[Double]
                val th    = dims.h.asInstanceOf[Double]
                val boxW  = tw + 2 * padding
                val boxH  = th + 2 * padding
                val boxX  = if (alignLeft) x else x - boxW
                val _     = doc.setFillColor(bgR, bgG, bgB)
                val _     = doc.rect(boxX, yTop, boxW, boxH, "F")
                val _     = doc.setTextColor(0, 0, 0)
                val textY = yTop + padding + th
                val textX = if (alignLeft) x + padding else x - padding
                val align = if (alignLeft) "left" else "right"
                val _     = doc.text(value, textX, textY, js.Dynamic.literal(align = align))
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.AddPage =>
              docOpt.foreach { doc =>
                drawPageNumberIfNeeded(doc, pageW, pageH, pageIndex, totalPages, printerMarginMm)
                val _ = doc.addPage()
                fillPageBackground(doc, pageW, pageH, bgR, bgG, bgB, printerMarginMm)
              }
              (docOpt, (pageW, pageH), pageIndex + 1)
            case Instruction.DrawPixelGrid(x0, y0, wMm, hMm, cols, rows, rgbFlat) =>
              docOpt.foreach { doc =>
                if (cols > 0 && rows > 0 && rgbFlat.length >= cols * rows * 3) {
                  val cellW = wMm / cols
                  val cellH = hMm / rows
                  (0 until rows).foreach { y =>
                    (0 until cols).foreach { x =>
                      val i = (y * cols + x) * 3
                      val r = rgbFlat(i)
                      val g = rgbFlat(i + 1)
                      val b = rgbFlat(i + 2)
                      val _ = doc.setFillColor(r, g, b)
                      val _ = doc.rect(x0 + x * cellW, y0 + y * cellH, cellW, cellH, "F")
                    }
                  }
                }
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.DrawStrokeRects(rects, r, g, b, lineWidthMm) =>
              docOpt.foreach { doc =>
                val _ = doc.setDrawColor(r, g, b)
                val _ = doc.setLineWidth(lineWidthMm)
                rects.foreach { case (x, y, w, h) => val _ = doc.rect(x, y, w, h, "S") }
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.FillRect(x, y, w, h, r, g, b) =>
              docOpt.foreach { doc =>
                val _ = doc.setFillColor(r, g, b)
                val _ = doc.rect(x, y, w, h, "F")
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.FillRectWithOpacity(x, y, w, h, r, g, b, opacity) =>
              docOpt.foreach { doc =>
                val _      = doc.saveGraphicsState()
                val gstate = js.Dynamic.newInstance(doc.selectDynamic("GState"))(js.Dynamic.literal(opacity = opacity))
                val _      = doc.setGState(gstate)
                val _      = doc.setFillColor(r, g, b)
                val _      = doc.rect(x, y, w, h, "F")
                val _      = doc.restoreGraphicsState()
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.DrawSwatchRow(x, y, r, g, b, count, swatchSize, gap, fontSizePt, strokeLineWidth) =>
              docOpt.foreach { doc =>
                val textStr   = "×" + count
                val _         = doc.setFontSize(fontSizePt)
                val dims      = doc.getTextDimensions(textStr).asInstanceOf[js.Dynamic]
                val th        = dims.h.asInstanceOf[Double]
                val baselineY = y + (swatchSize + th) / 2
                val _         = doc.setFillColor(r, g, b)
                val _         = doc.rect(x, y, swatchSize, swatchSize, "F")
                if (strokeLineWidth > 0) {
                  val _ = doc.setDrawColor(0, 0, 0)
                  val _ = doc.setLineWidth(strokeLineWidth)
                  val _ = doc.rect(x, y, swatchSize, swatchSize, "S")
                }
                val _ = doc.setTextColor(0, 0, 0)
                val _ = doc.text(textStr, x + swatchSize + gap, baselineY)
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.RoundedFillRect(x, y, w, h, radius, r, g, b) =>
              docOpt.foreach { doc =>
                val _ = doc.setFillColor(r, g, b)
                val _ = doc.roundedRect(x, y, w, h, radius, radius, "F")
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.RoundedStrokeRect(x, y, w, h, radius, strokeR, strokeG, strokeB, lineWidth) =>
              docOpt.foreach { doc =>
                val _ = doc.setDrawColor(strokeR, strokeG, strokeB)
                val _ = doc.setLineWidth(lineWidth)
                val _ = doc.roundedRect(x, y, w, h, radius, radius, "S")
              }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.Save(filename) =>
              docOpt.foreach { doc =>
                drawPageNumberIfNeeded(doc, pageW, pageH, pageIndex, totalPages, printerMarginMm)
                val _ = doc.save(filename)
              }
              (docOpt, (pageW, pageH), pageIndex)
          }
        }
    }

  private def getJsPDFConstructor: Option[js.Dynamic] = {
    val jspdfObj = js.Dynamic.global.selectDynamic("jspdf")
    if (js.typeOf(jspdfObj) == "undefined") None
    else {
      val ctorKey = "js" + "PDF"
      val ctor    = jspdfObj.selectDynamic(ctorKey)
      if (js.typeOf(ctor) != "undefined") Some(ctor.asInstanceOf[js.Dynamic]) else None
    }
  }
}
