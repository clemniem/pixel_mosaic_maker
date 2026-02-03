package clemniem.screens

import cats.effect.IO
import clemniem.{Color, NavigateNext, Screen, ScreenId, ScreenOutput, StoredPalette, StorageKeys}
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Palette editor: list of colors (default 4, max 16), editable by hex or color picker. */
object PaletteScreen extends Screen {
  type Model = PaletteModel
  type Msg   = PaletteMsg | NavigateNext

  val screenId: ScreenId = ScreenId.PaletteId

  private val defaultColors: Vector[Color] =
    Vector(Color(0, 0, 0), Color(255, 255, 255), Color(200, 50, 50), Color(50, 120, 200))
  private val maxColors = 16

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = previous match {
      case Some(ScreenOutput.EditPalette(stored)) =>
        val colors =
          if (stored.colors.nonEmpty) stored.colors
          else defaultColors
        PaletteModel(
          name = stored.name,
          colors = colors,
          editingId = Some(stored.id)
        )
      case Some(ScreenOutput.NewPaletteFromImage(name, colors)) =>
        val cs = if (colors.nonEmpty) colors else defaultColors
        PaletteModel(name = name, colors = cs, editingId = None)
      case _ =>
        PaletteModel(
          name = "Unnamed palette",
          colors = defaultColors,
          editingId = None
        )
    }
    (model, Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PaletteMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case PaletteMsg.SetColorHex(idx, raw) =>
      if (idx >= 0 && idx < model.colors.length) {
        val color = Color.fromHex(raw)
        val next  = model.copy(colors = model.colors.updated(idx, color))
        (next, Cmd.None)
      } else (model, Cmd.None)

    case PaletteMsg.SetColorFromPicker(idx, hex) =>
      if (idx >= 0 && idx < model.colors.length) {
        val color = Color.fromHex(hex)
        val next  = model.copy(colors = model.colors.updated(idx, color))
        (next, Cmd.None)
      } else (model, Cmd.None)

    case PaletteMsg.AddColor =>
      if (model.colors.length >= maxColors) (model, Cmd.None)
      else
        (model.copy(colors = model.colors :+ Color(128, 128, 128)), Cmd.None)

    case PaletteMsg.RemoveColor(idx) =>
      if (model.colors.length <= 1) (model, Cmd.None)
      else if (idx >= 0 && idx < model.colors.length) {
        val next = model.copy(colors = model.colors.patch(idx, Nil, 1))
        (next, Cmd.None)
      } else (model, Cmd.None)

    case PaletteMsg.Save =>
      val cmd = LocalStorageUtils.loadList(StorageKeys.palettes)(
        PaletteMsg.LoadedForSave.apply,
        _ => PaletteMsg.LoadedForSave(Nil),
        (_, _) => PaletteMsg.LoadedForSave(Nil)
      )
      (model, cmd)

    case PaletteMsg.LoadedForSave(list) =>
      val id     = model.editingId.getOrElse("palette-" + js.Date.now().toLong)
      val stored = StoredPalette(id = id, name = model.name, colors = model.colors)
      val newList = model.editingId match {
        case Some(editId) => list.filterNot(_.id == editId) :+ stored
        case None         => list :+ stored
      }
      val saveCmd = LocalStorageUtils.saveList(StorageKeys.palettes, newList)(
        _ => NavigateNext(ScreenId.PalettesId, None),
        (_, _) => PaletteMsg.SaveFailed
      )
      (model, saveCmd)

    case PaletteMsg.SaveFailed =>
      (model, Cmd.None)

    case PaletteMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.PalettesId, None)))

    case PaletteMsg.NoOp =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(
      style := "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1rem;"
    )(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
        h2(style := "margin: 0;")(text("Palette")),
        div(style := "display: flex; align-items: center; gap: 8px;")(
          button(style := "padding: 6px 12px; cursor: pointer;", onClick(PaletteMsg.Back))(
            text("← Palettes")
          ),
          input(
            `type` := "text",
            placeholder := "Name",
            value := model.name,
            onInput(PaletteMsg.SetName.apply),
            style := "padding: 6px 10px; width: 12rem; border: 1px solid #ccc; border-radius: 4px;"
          ),
          button(
            style := "padding: 6px 14px; cursor: pointer; background: #2e7d32; color: #fff; border: none; border-radius: 4px; font-weight: 500;",
            onClick(PaletteMsg.Save)
          )(text("Save"))
        )
      ),
      p(style := "color: #444; margin-bottom: 1rem;")(
        text(s"Colors (${model.colors.length}/$maxColors). Edit by hex or use the color picker. Add or remove colors.")
      ),
      div(style := "display: flex; flex-direction: column; gap: 10px; margin-bottom: 1rem;")(
        model.colors.zipWithIndex.toList.map { case (color, idx) =>
          colorRow(idx, color)
        }*
      ),
      addColorButton(model.colors.length >= maxColors)
    )

  private def colorRow(idx: Int, color: Color): Html[Msg] =
    div(
      style := "display: flex; align-items: center; flex-wrap: wrap; gap: 8px; padding: 8px; background: #f5f5f5; border-radius: 4px;"
    )(
      div(
        style := s"width: 36px; height: 36px; border-radius: 4px; border: 1px solid #333; background: ${color.toHex}; flex-shrink: 0;"
      )(),
      span(style := "font-size: 0.85rem; min-width: 2.5rem;")(text(s"${idx + 1}")),
      input(
        `type` := "text",
        value := color.toHex,
        onInput(s => PaletteMsg.SetColorHex(idx, s)),
        style := "width: 6rem; padding: 4px; font-family: monospace; font-size: 0.9rem;"
      ),
      input(
        `type` := "color",
        value := color.toHex,
        onInput(s => PaletteMsg.SetColorFromPicker(idx, s)),
        style := "width: 40px; height: 36px; padding: 0; border: 1px solid #999; cursor: pointer; border-radius: 4px;"
      ),
      button(
        onClick(PaletteMsg.RemoveColor(idx)),
        style := "padding: 4px 8px; cursor: pointer;"
      )(text("− Remove"))
    )

  private def addColorButton(isDisabled: Boolean): Html[Msg] =
    button(
      style := styleAddColor(isDisabled),
      onClick(PaletteMsg.AddColor)
    )(text("+ Add color"))

  private def styleAddColor(disabled: Boolean): String =
    if (disabled)
      "padding: 8px 16px; cursor: not-allowed; background: #ccc; color: #666; border: none; border-radius: 4px;"
    else
      "padding: 8px 16px; cursor: pointer; background: #fff; border: 1px solid #555; border-radius: 4px;"
}

final case class PaletteModel(
    name: String,
    colors: Vector[Color],
    editingId: Option[String]
)

enum PaletteMsg:
  case SetName(name: String)
  case SetColorHex(idx: Int, value: String)
  case SetColorFromPicker(idx: Int, hex: String)
  case AddColor
  case RemoveColor(idx: Int)
  case Save
  case LoadedForSave(list: List[StoredPalette])
  case SaveFailed
  case Back
  case NoOp
