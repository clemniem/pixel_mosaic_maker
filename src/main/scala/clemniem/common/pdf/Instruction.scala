package clemniem.common.pdf

/** Simple instructions for PDF generation. All jsPDF details are hidden; [[JsPDF]] runs these. */
sealed trait Instruction

object Instruction {
  /** Create a new document with one page of the given size (width and height in mm). */
  case class PageSize(widthMm: Double, heightMm: Double) extends Instruction

  /** Set font size in points. */
  case class FontSize(pt: Int) extends Instruction

  /** Draw text at (x, y) in mm. */
  case class Text(xMm: Double, yMm: Double, value: String) extends Instruction

  /** Trigger browser download with the given filename. */
  case class Save(filename: String) extends Instruction
}
