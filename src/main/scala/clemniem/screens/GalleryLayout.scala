package clemniem.screens

import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared layout for gallery screens: root container, header (title + back button), and content. */
object GalleryLayout {

  /** @param title       Screen title (e.g. "Palettes", "Build configs")
    * @param backButton  Back button HTML (e.g. ← Overview)
    * @param content     Main content: loading state, empty state, or list + actions
    * @param shortHeader If true, use smaller margin below header
    */
  def apply[Msg](
      title: String,
      backButton: Html[Msg],
      content: Html[Msg],
      shortHeader: Boolean
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container")(
      div(`class` := headerClass)(
        h1(`class` := "screen-title")(text(title)),
        backButton
      ),
      content
    )
  }
}
