package clemniem.screens

import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared layout for gallery screens: root container, header (title + back button), and content. */
object GalleryLayout {

  /** CSS class for the content area so items don't touch container borders. Use with gallery-list for the list wrapper. */
  val galleryContentClass = "gallery-content"

  /** CSS class for the vertical list of gallery cards + actions (gap between items, no touching). */
  val galleryListClass = "gallery-list"

  /** @param title       Screen title (e.g. "Palettes", "Build configs")
    * @param backButton  Back button HTML (e.g. ← Overview)
    * @param content     Main content: loading state, empty state, or list + actions (will be wrapped in gallery-content)
    * @param shortHeader If true, use smaller margin below header
    * @param nextButton  Optional "Next →" button (navigates to next screen in overview order)
    */
  def apply[Msg](
      title: String,
      backButton: Html[Msg],
      content: Html[Msg],
      shortHeader: Boolean,
      nextButton: Option[Html[Msg]]
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    val headerButtons = nextButton match {
      case Some(next) => div(`class` := "flex-row", style := "gap: 0.5rem;")(backButton, next)
      case None       => backButton
    }
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container")(
      div(`class` := headerClass)(
        h1(`class` := "screen-title")(text(title)),
        headerButtons
      ),
      div(`class` := galleryContentClass)(content)
    )
  }
}
