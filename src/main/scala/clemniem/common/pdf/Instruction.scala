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

  /** Draw text with a filled background (e.g. NES-style title over frame). When alignLeft, xMm is left edge; else xMm is right edge. yTopMm = top of background box. */
  case class TextWithBackground(
      xMm: Double,
      yTopMm: Double,
      value: String,
      fontSizePt: Int,
      paddingMm: Double,
      alignLeft: Boolean,
      bgR: Int,
      bgG: Int,
      bgB: Int
  ) extends Instruction

  /** Add a new page (same size as document). Must be used after at least one PageSize. */
  case object AddPage extends Instruction

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

  /** Draw one pixel-count row: swatch square + "× count" text. Text is vertically centered with the swatch. Reusable for full overview and chapter pages. */
  case class DrawSwatchRow(
      xMm: Double,
      yMm: Double,
      swatchR: Int,
      swatchG: Int,
      swatchB: Int,
      count: Int,
      swatchSizeMm: Double,
      gapMm: Double,
      fontSizePt: Int
  ) extends Instruction

  /** Draw a filled rounded rectangle (e.g. cover frame). */
  case class RoundedFillRect(xMm: Double, yMm: Double, widthMm: Double, heightMm: Double, radiusMm: Double, r: Int, g: Int, b: Int) extends Instruction

  /** Draw a stroked rounded rectangle (e.g. cover frame outline). */
  case class RoundedStrokeRect(xMm: Double, yMm: Double, widthMm: Double, heightMm: Double, radiusMm: Double, strokeR: Int, strokeG: Int, strokeB: Int, lineWidthMm: Double) extends Instruction

  /** Trigger browser download with the given filename. */
  case class Save(filename: String) extends Instruction
}
