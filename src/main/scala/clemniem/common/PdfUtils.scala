package clemniem.common

import cats.effect.IO
import clemniem.{GridConfig, PixelPic}
import clemniem.common.pdf.{Instruction, JsPDF, PdfLayout}

/** Request for the book PDF: title and optional mosaic (pic + grid). Both Print buttons use this. */
final case class PrintBookRequest(
    title: String,
    mosaicPicAndGridOpt: Option[(PixelPic, GridConfig)]
)

/** High-level PDF helpers. Build [[Instruction]]s and run them via [[JsPDF]]. */
object PdfUtils {

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
    val (pageW, pageH)   = (210.0, 297.0) // A4
    val marginLR         = 20.0
    val marginTB         = 25.0
    val availableW       = pageW - 2 * marginLR
    val availableH       = pageH - 2 * marginTB
    val overviewInstructions = mosaicPicAndGridOpt match {
      case Some((pic, grid)) =>
        val (pw, ph, rgbFlat) = pixelPicToRgbFlat(pic)
        val scale             = (availableW / pw).min(availableH / ph)
        val imageW            = pw * scale
        val imageH            = ph * scale
        val x0                = marginLR + (availableW - imageW) / 2
        val y0                = marginTB + (availableH - imageH) / 2
        val gridRectsMm = grid.parts.toList.map { part =>
          (x0 + part.x * scale, y0 + part.y * scale, part.width * scale, part.height * scale)
        }
        List(
          Instruction.AddPage,
          Instruction.DrawPixelGrid(x0, y0, imageW, imageH, pw, ph, rgbFlat),
          Instruction.DrawStrokeRects(gridRectsMm, 255, 0, 0)
        )
      case None => Nil
    }
    val instructions =
      PdfLayout.coverInstructions(title) ++ overviewInstructions :+ Instruction.Save("mosaic-book.pdf")
    JsPDF.run(instructions)
  }

  /** Row-major flat RGB (3 ints per pixel) for use in DrawPixelGrid. */
  private def pixelPicToRgbFlat(pic: PixelPic): (Int, Int, Vector[Int]) = {
    val flat = pic.pixels.iterator.flatMap { i =>
      val p = pic.paletteLookup(i)
      Vector(p.r, p.g, p.b)
    }.toVector
    (pic.width, pic.height, flat)
  }
}
