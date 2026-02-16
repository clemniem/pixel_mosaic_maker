package clemniem.screens

import clemniem.Color
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared palette strip UI: NES-style row of color swatches. Use previewInline for display-only, or swatches inside a
  * button for clickable.
  */
object PaletteStripView {

  /** Renders the small swatch elements (for use inside a button or preview wrapper). */
  def swatches[Msg](colors: List[Color]): Seq[Html[Msg]] =
    colors.map(c => div(`class` := "palette-swatch-small", style := s"background: ${c.toHex};")())

  /** NES-style palette strip that is not clickable (same look as gallery palette button). Use for upload preview and
    * palettes gallery.
    */
  def previewInline[Msg](colors: List[Color]): Html[Msg] =
    div(`class` := s"${NesCss.btn} palette-button-inline palette-preview-inline")(
      swatches(colors.take(16))*
    )
}
