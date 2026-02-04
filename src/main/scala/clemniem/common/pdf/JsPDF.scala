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
