package clemniem.common.pdf

import org.scalajs.dom.console
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

/**
 * Runs PDF [[Instruction]]s using the jsPDF library (must be loaded on window.jspdf).
 * All jsPDF API access is confined to this object.
 */
object JsPDF {

  /** Execute instructions after a short delay (so jsPDF global is ready). Returns false if jsPDF was not found. */
  def run(instructions: List[Instruction]): Unit = {
    val _ = setTimeout(0)(runNow(instructions))
  }

  def runNow(instructions: List[Instruction]): Unit =
    getJsPDFConstructor match {
      case None =>
        console.warn("JsPDF: jsPDF not found on window. Add script tag for jspdf.umd.min.js.")
      case Some(ctor) =>
        val _ = instructions.foldLeft(Option.empty[js.Dynamic]) { (docOpt, inst) =>
          inst match {
            case Instruction.PageSize(w, h) =>
              val _ = docOpt
              val opts = js.Dynamic.literal(
                orientation = "p",
                unit = "mm",
                format = js.Array(w, h)
              )
              Some(js.Dynamic.newInstance(ctor)(opts))
            case Instruction.FontSize(pt) =>
              docOpt.foreach { doc => val _ = doc.setFontSize(pt) }
              docOpt
            case Instruction.Text(x, y, value) =>
              docOpt.foreach { doc => val _ = doc.text(value, x, y) }
              docOpt
            case Instruction.AddPage =>
              docOpt.foreach { doc => val _ = doc.addPage() }
              docOpt
            case Instruction.AddPageWithSize(w, h) =>
              docOpt.foreach { doc => val _ = doc.addPage(js.Array(w, h), "p") }
              docOpt
            case Instruction.AddImage(dataUrl, x, y, w, h) =>
              docOpt.foreach { doc =>
                val format = if (dataUrl.contains("image/svg+xml")) "SVG" else "PNG"
                val _      = doc.addImage(dataUrl, format, x, y, w, h)
              }
              docOpt
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
              docOpt
            case Instruction.DrawStrokeRects(rects, r, g, b, lineWidthMm) =>
              docOpt.foreach { doc =>
                val _ = doc.setDrawColor(r, g, b)
                val _ = doc.setLineWidth(lineWidthMm)
                rects.foreach { case (x, y, w, h) => val _ = doc.rect(x, y, w, h, "S") }
              }
              docOpt
            case Instruction.FillRect(x, y, w, h, r, g, b) =>
              docOpt.foreach { doc =>
                val _ = doc.setFillColor(r, g, b)
                val _ = doc.rect(x, y, w, h, "F")
              }
              docOpt
            case Instruction.Save(filename) =>
              docOpt.foreach { doc => val _ = doc.save(filename) }
              docOpt
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
