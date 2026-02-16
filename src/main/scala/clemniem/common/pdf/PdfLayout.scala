package clemniem.common.pdf

/** Pure layout functions: build [[Instruction]] lists for each part of the PDF book. No jsPDF dependency. Uses
  * [[PdfLayoutConfig]] for layout values.
  */
object PdfLayout {

  private val defaultConfig = PdfLayoutConfig.default

  /** All pages use 20×20 cm (from default config). */
  val pageSizeMm: Double = defaultConfig.global.pageSizeMm

  /** Cover title font size (from default config). */
  val coverTitleFontSize: Int = defaultConfig.cover.titleFontSizePt

  /** Instructions for the cover page (title only, no mosaic). Does not include Save. printerMarginMm offsets content so
    * margin stays white.
    */
  def coverInstructions(title: String, printerMarginMm: Double, config: PdfLayoutConfig): List[Instruction] =
    List(
      Instruction.PageSize(config.global.pageSizeMm, config.global.pageSizeMm),
      Instruction.FontSize(config.cover.titleFontSizePt),
      Instruction.Text(config.cover.titleOnlyXMm + printerMarginMm, config.cover.titleOnlyYMm + printerMarginMm, title)
    )
}
