package clemniem.common.pdf

/** Pure layout functions: build [[Instruction]] lists for each part of the PDF book. No jsPDF dependency. */
object PdfLayout {

  /** All pages use 20×20 cm. */
  val pageSizeMm = 200.0

  private val coverTitleY = 95.0
  private val coverTitleX = 25.0
  private val coverTitleFontSize = 28

  /** Instructions for the cover page: 20×20 cm with centered title. Does not include Save. */
  def coverInstructions(title: String): List[Instruction] =
    List(
      Instruction.PageSize(pageSizeMm, pageSizeMm),
      Instruction.FontSize(coverTitleFontSize),
      Instruction.Text(coverTitleX, coverTitleY, title)
    )
}
