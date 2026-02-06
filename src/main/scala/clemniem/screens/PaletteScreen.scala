package clemniem.screens

import cats.effect.IO
import clemniem.{Color, NavigateNext, Screen, ScreenId, ScreenOutput, StoredPalette, StorageKeys}
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Palette editor: list of colors (default 4, max 16), editable by hex or color picker; reorder with arrows. */
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

    case PaletteMsg.MoveColorUp(idx) =>
      if (idx <= 0 || idx >= model.colors.length) (model, Cmd.None)
      else {
        val c = model.colors
        val next = c.updated(idx, c(idx - 1)).updated(idx - 1, c(idx))
        (model.copy(colors = next), Cmd.None)
      }

    case PaletteMsg.MoveColorDown(idx) =>
      if (idx < 0 || idx >= model.colors.length - 1) (model, Cmd.None)
      else {
        val c = model.colors
        val next = c.updated(idx, c(idx + 1)).updated(idx + 1, c(idx))
        (model.copy(colors = next), Cmd.None)
      }

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

  def view(model: Model): Html[Msg] = {
    val root = s"${NesCss.container} ${NesCss.containerRounded} screen-container"
    div(`class` := root)(
      div(`class` := "screen-header")(
        h2(`class` := "screen-title")(text("Palette")),
        div(`class` := "flex-row")(
          button(`class` := NesCss.btn, onClick(PaletteMsg.Back))(text("← Palettes")),
          button(`class` := NesCss.btnPrimary, onClick(PaletteMsg.Save))(text("Save"))
        )
      ),
      div(`class` := "palette-name-row")(
        input(
          id := "palette-name",
          `type` := "text",
          placeholder := "Name",
          value := model.name,
          onInput(PaletteMsg.SetName.apply),
          `class` := s"${NesCss.input} palette-name-input",
        )
      ),
      p(`class` := NesCss.text, style := "margin-bottom: 1rem;")(
        text(s"Colors (${model.colors.length}/$maxColors). Edit hex or use picker. Use arrows to reorder.")
      ),
      div(`class` := "palette-edit-list")(
        model.colors.zipWithIndex.toList.map { case (color, idx) =>
          colorRow(idx, color, model.colors.length)
        }*
      ),
      if (model.colors.length >= maxColors) div()()
      else
        button(`class` := NesCss.btn, onClick(PaletteMsg.AddColor))(text("+ Add color"))
    )
  }

  private def colorRow(idx: Int, color: Color, total: Int): Html[Msg] =
    div(`class` := "palette-edit-row")(
      div(`class` := "palette-edit-arrows")(
        button(
          `class` := (if (idx <= 0) s"${NesCss.btn} btn-disabled" else NesCss.btn),
          onClick(PaletteMsg.MoveColorUp(idx))
        )(text("↑")),
        button(
          `class` := (if (idx >= total - 1) s"${NesCss.btn} btn-disabled" else NesCss.btn),
          onClick(PaletteMsg.MoveColorDown(idx))
        )(text("↓"))
      ),
      div(
        `class` := "palette-edit-swatch",
        style := s"background: ${color.toHex};"
      )(),
      span(`class` := NesCss.text, style := "min-width: 1.25rem;")(text(s"${idx + 1}.")),
      input(
        id := s"palette-hex-$idx",
        `type` := "text",
        value := color.toHex,
        onInput(s => PaletteMsg.SetColorHex(idx, s)),
        `class` := s"${NesCss.input} palette-edit-hex",
      ),
      input(
        id := s"palette-picker-$idx",
        `type` := "color",
        value := color.toHex,
        onInput(s => PaletteMsg.SetColorFromPicker(idx, s)),
        `class` := "palette-edit-picker",
      ),
      button(`class` := s"${NesCss.btnError} palette-edit-remove", onClick(PaletteMsg.RemoveColor(idx)))(
        text("×")
      )
    )
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
  case MoveColorUp(idx: Int)
  case MoveColorDown(idx: Int)
  case Save
  case LoadedForSave(list: List[StoredPalette])
  case SaveFailed
  case Back
  case NoOp
