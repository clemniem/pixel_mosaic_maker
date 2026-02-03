package clemniem.screens

import tyrian.Html.*
import tyrian.*

/** Shared empty-state UI for gallery screens: dashed box, message, and primary action button. */
object GalleryEmptyState {

  /** @param emptyText   Paragraph text (e.g. "No build configs yet.")
    * @param buttonLabel Full button label (e.g. "+ Create BuildConfig" or "Upload")
    * @param createMsg   Message to emit when the button is clicked
    */
  def apply[Msg](emptyText: String, buttonLabel: String, createMsg: Msg): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text(emptyText)),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(buttonLabel))
    )
}
