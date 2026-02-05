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

  /** Add a new page (same size as document). Must be used after at least one PageSize. */
  case object AddPage extends Instruction

  /** Add a new page with the given size in mm (e.g. 200×200 for 20×20 cm). */
  case class AddPageWithSize(widthMm: Double, heightMm: Double) extends Instruction

  /** Embed an image (data URL, e.g. PNG or SVG). Position and size in mm. */
  case class AddImage(dataUrl: String, xMm: Double, yMm: Double, widthMm: Double, heightMm: Double) extends Instruction

  /** Draw a pixel grid as small rects (gbcamutil-style). rgbFlat: row-major, 3 ints per pixel (r,g,b). */
  case class DrawPixelGrid(
      xMm: Double,
      yMm: Double,
      widthMm: Double,
      heightMm: Double,
      pixelWidth: Int,
      pixelHeight: Int,
      rgbFlat: Vector[Int]
  ) extends Instruction

  /** Draw stroke-only rects (e.g. plate grid overlay). Each (xMm, yMm, widthMm, heightMm). Line width in mm. */
  case class DrawStrokeRects(
      rectsMm: List[(Double, Double, Double, Double)],
      strokeR: Int,
      strokeG: Int,
      strokeB: Int,
      lineWidthMm: Double = 0.25
  ) extends Instruction

  /** Draw a filled rectangle (e.g. color swatch). */
  case class FillRect(xMm: Double, yMm: Double, widthMm: Double, heightMm: Double, r: Int, g: Int, b: Int) extends Instruction

  /** Trigger browser download with the given filename. */
  case class Save(filename: String) extends Instruction
}
