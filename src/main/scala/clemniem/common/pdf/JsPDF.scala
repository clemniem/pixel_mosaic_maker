package clemniem.common.pdf

import org.scalajs.dom.console
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

/**
 * Runs PDF [[Instruction]]s using the jsPDF library (must be loaded on window.jspdf).
 * All jsPDF API access is confined to this object.
 */
object JsPDF {

  private def fillPageBackground(doc: js.Dynamic, w: Double, h: Double, r: Int, g: Int, b: Int, printerMarginMm: Double): Unit = {
    val _ = doc.setFillColor(r, g, b)
    if (printerMarginMm <= 0) {
      val _ = doc.rect(0, 0, w, h, "F")
    } else {
      val x = printerMarginMm
      val y = printerMarginMm
      val rw = (w - 2 * printerMarginMm).max(0)
      val rh = (h - 2 * printerMarginMm).max(0)
      val _ = doc.rect(x, y, rw, rh, "F")
    }
  }

  /** Execute instructions after a short delay (so jsPDF global is ready). Page background RGB 0–255. printerMarginMm: border left white on each side. */
  def run(instructions: List[Instruction], bgR: Int, bgG: Int, bgB: Int, printerMarginMm: Double): Unit = {
    val _ = setTimeout(0)(runNow(instructions, bgR, bgG, bgB, printerMarginMm))
  }

  /** Total page count: 1 + number of AddPage (Save is not a page). */
  private def countPages(instructions: List[Instruction]): Int =
    1 + instructions.count { case Instruction.AddPage => true; case _ => false }

  /** Draw page number in outer corner for booklet: first 3 and last 2 pages have no number; left/right alternate (even = left). */
  private def drawPageNumberIfNeeded(
      doc: js.Dynamic,
      pageW: Double,
      pageH: Double,
      pageIndex0Based: Int,
      totalPages: Int,
      printerMarginMm: Double
  ): Unit = {
    val pageNum = pageIndex0Based + 1
    if (pageNum >= 4 && pageNum <= totalPages - 2) {
      val leftSide = (pageNum % 2) == 0
      val _       = doc.setFontSize(9)
      val _       = doc.setTextColor(0, 0, 0)
      val y       = pageH - printerMarginMm - 5
      if (leftSide) {
        val x = printerMarginMm + 2
        val _ = doc.text(pageNum.toString, x, y)
      } else {
        val x = pageW - printerMarginMm - 2
        val _ = doc.text(pageNum.toString, x, y, js.Dynamic.literal(align = "right"))
      }
    }
  }

  def runNow(instructions: List[Instruction], bgR: Int, bgG: Int, bgB: Int, printerMarginMm: Double): Unit =
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
              (Some(doc), (w, h), 0)
            case Instruction.FontSize(pt) =>
              docOpt.foreach { doc => val _ = doc.setFontSize(pt) }
              (docOpt, (pageW, pageH), pageIndex)
            case Instruction.Text(x, y, value) =>
              docOpt.foreach { doc => val _ = doc.text(value, x, y) }
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
