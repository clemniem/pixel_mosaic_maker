package clemniem.common.pdf

/** Pure layout functions: build [[Instruction]] lists for each part of the PDF book. No jsPDF dependency. */
object PdfLayout {

  /** All pages use 20×20 cm. */
  val pageSizeMm = 200.0

  private val coverTitleX   = 25.0
  private val coverTitleY   = 95.0   // used when no mosaic (title-only cover)
  val coverTitleFontSize    = 28

  /** Instructions for the cover page (title only, no mosaic). Does not include Save. printerMarginMm offsets content so margin stays white. */
  def coverInstructions(title: String, printerMarginMm: Double): List[Instruction] =
    List(
      Instruction.PageSize(pageSizeMm, pageSizeMm),
      Instruction.FontSize(coverTitleFontSize),
      Instruction.Text(coverTitleX + printerMarginMm, coverTitleY + printerMarginMm, title)
    )
}
