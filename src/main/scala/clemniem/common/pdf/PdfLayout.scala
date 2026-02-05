package clemniem.common.pdf

/** Pure layout functions: build [[Instruction]] lists for each part of the PDF book. No jsPDF dependency. */
object PdfLayout {

  private val a4WidthMm  = 210.0
  private val a4HeightMm  = 297.0
  private val coverTitleY = 140.0
  private val coverTitleX = 25.0
  private val coverTitleFontSize = 28

  /** Instructions for the cover page: one A4 page with the title. Does not include Save. */
  def coverInstructions(title: String): List[Instruction] =
    List(
      Instruction.PageSize(a4WidthMm, a4HeightMm),
      Instruction.FontSize(coverTitleFontSize),
      Instruction.Text(coverTitleX, coverTitleY, title)
    )
}
