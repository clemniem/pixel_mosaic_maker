package clemniem.screens

import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared screen header: title on top, then button row, then optional name row. Use for editor screens. */
object ScreenHeader {

  /** Build the standard header (title, button row, optional name row).
    * @param title       Screen title (e.g. "Build config", "Palette")
    * @param buttonRow   HTML for the action buttons (Back, Save, etc.)
    * @param nameRow     Optional name input row (entity name); use [[nameRowInput]] to build it
    * @param shortHeader If true, add screen-header--short for tighter margin
    */
  def apply[Msg](
      title: String,
      buttonRow: Html[Msg],
      nameRow: Option[Html[Msg]],
      shortHeader: Boolean
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    val children   = Seq(h2(`class` := "screen-title")(text(title)), buttonRow) ++ nameRow.toSeq
    div(`class` := headerClass)(children*)
  }

  /** Standard name input row for creation/editor screens. Input is full width of the header. */
  def nameRowInput[Msg](
      nameValue: String,
      setMsg: String => Msg,
      inputId: Option[String],
      extraRowClass: String
  ): Html[Msg] = {
    val rowClass = s"${NesCss.field} screen-header-name-row $extraRowClass".trim
    val attrs   = Seq(
      `type` := "text",
      placeholder := "Name",
      value := nameValue,
      onInput(setMsg),
      `class` := s"${NesCss.input} screen-header-name-input"
    ) ++ inputId.map(i => id := i)
    div(`class` := rowClass)(input(attrs*))
  }
}
