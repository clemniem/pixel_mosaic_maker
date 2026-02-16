package clemniem.screens

import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared empty-state UI for gallery screens: dashed box, message, and primary action button. */
object GalleryEmptyState {

  /** @param emptyText
    *   Paragraph text (e.g. "No build configs yet.")
    * @param buttonLabel
    *   Full button label (e.g. "+ Create BuildConfig" or "Upload")
    * @param createMsg
    *   Message to emit when the button is clicked
    */
  def apply[Msg](emptyText: String, buttonLabel: String, createMsg: Msg): Html[Msg] =
    div(`class` := s"${NesCss.container} empty-state")(
      p(`class` := NesCss.text)(text(emptyText)),
      button(`class` := NesCss.btnPrimary, onClick(createMsg))(text(buttonLabel))
    )
}
