package clemniem.common.pdf

import org.scalajs.dom.console
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout
import scala.scalajs.js.typedarray.Uint8Array

/**
 * Runs PDF [[Instruction]]s using the jsPDF library (must be loaded on window.jspdf).
 * All jsPDF API access is confined to this object.
 * Uses "Press Start 2P" (same as the website) when the font is loaded from assets.
 */
object JsPDF {

  private val pressStart2PFontUrl  = "./assets/PressStart2P-Regular.ttf"
  private val pressStart2PVfsName  = "PressStart2P-Regular.ttf"
  private val pressStart2PFontName = "Press Start 2P"

  private def arrayBufferToBase64(ab: js.typedarray.ArrayBuffer): String = {
    val bytes = new Uint8Array(ab)
    val len   = bytes.length
    @scala.annotation.tailrec
    def loop(i: Int, sb: StringBuilder): String =
      if (i >= len) js.Dynamic.global.btoa(sb.result()).asInstanceOf[String]
      else {
        sb.append((bytes(i) & 0xff).toChar)
        loop(i + 1, sb)
      }
    loop(0, new StringBuilder(len))
  }

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

  /** Execute instructions after a short delay (so jsPDF global is ready). Fetches Press Start 2P font from assets, then runs; falls back to default font if fetch fails. */
  def run(instructions: List[Instruction], bgR: Int, bgG: Int, bgB: Int, printerMarginMm: Double): Unit = {
    val runWithFont    = (base64: String) => runNow(instructions, bgR, bgG, bgB, printerMarginMm, Some(base64))
    val runWithoutFont = () => runNow(instructions, bgR, bgG, bgB, printerMarginMm, None)
    val _ = setTimeout(0) {
      val fetchFn = js.Dynamic.global.selectDynamic("fetch").asInstanceOf[js.Function1[String, js.Dynamic]]
      val promise = fetchFn(pressStart2PFontUrl)
      val _ = promise
        .`then`(js.defined { (r: js.Dynamic) => r.arrayBuffer() })
        .`then`(js.defined { (ab: js.Dynamic) =>
          runWithFont(arrayBufferToBase64(ab.asInstanceOf[js.typedarray.ArrayBuffer]))
          ()
        })
        .`catch`(js.defined { (_: js.Dynamic) =>
          runWithoutFont()
          ()
        })
      ()
    }
  }

  /** Total page count: 1 + number of AddPage (Save is not a page). */
  private def countPages(instructions: List[Instruction]): Int =
    1 + instructions.count { case Instruction.AddPage => true; case _ => false }

  /** Draw page number in outer corner for booklet: first 2 PDF pages (cover) and last 2 (cover back) have no number.
    * Open the cover: first right-hand page = content page 1, back of it = 2, etc. Even content page = left, odd = right. */
  private def drawPageNumberIfNeeded(
      doc: js.Dynamic,
      pageW: Double,
      pageH: Double,
      pageIndex0Based: Int,
      totalPages: Int,
      printerMarginMm: Double
  ): Unit = {
    if (pageIndex0Based >= 2 && pageIndex0Based <= totalPages - 3) {
      val contentPageNum = pageIndex0Based - 1 // first right-hand = 1, back of it = 2, ...
      val leftSide      = (contentPageNum % 2) == 0
      val _             = doc.setFontSize(9)
      val _             = doc.setTextColor(0, 0, 0)
      val y             = pageH - printerMarginMm - 5
      if (leftSide) {
        val x = printerMarginMm + 2
        val _ = doc.text(contentPageNum.toString, x, y)
      } else {
        val x = pageW - printerMarginMm - 2
        val _ = doc.text(contentPageNum.toString, x, y, js.Dynamic.literal(align = "right"))
      }
    }
  }

  def runNow(instructions: List[Instruction], bgR: Int, bgG: Int, bgB: Int, printerMarginMm: Double, fontBase64Opt: Option[String]): Unit =
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
                val _ = doc.setFont(pressStart2PFontName, "normal")
              }
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
