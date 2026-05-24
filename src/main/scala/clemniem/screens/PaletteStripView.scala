package clemniem.screens

import clemniem.{Color, PixelPic}
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

  /** Per-color pixel count row for a cropped PixelPic patch. Renders a color swatch and pixel count for each color
    * present in the patch, sorted from fewest to most pixels. Renders nothing when no patch is available.
    */
  def colorCountOverview[Msg](patchOpt: Option[PixelPic]): Html[Msg] =
    patchOpt match {
      case None => div()()
      case Some(patch) =>
        val sorted = patch.palette.toVector.sortBy(_._2)
        div(`class` := "color-count-overview")(
          sorted.map { case (paletteIndex, count) =>
            val px  = patch.paletteLookup(paletteIndex)
            val hex = Color(px.r, px.g, px.b).toHex
            div(`class` := "color-count-row")(
              div(`class` := "color-count-swatch", style := s"background: $hex;")(),
              span(text(s"$count px"))
            )
          }*
        )
    }
}
