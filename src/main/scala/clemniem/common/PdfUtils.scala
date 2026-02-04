package clemniem.common

import cats.effect.IO
import clemniem.common.pdf.{Instruction, JsPDF}

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
}
