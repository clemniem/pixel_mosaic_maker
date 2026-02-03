package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredPalette}
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

/** Gallery of saved palettes. Empty state: "+ Create Palette". Edit opens palette editor. */
object PalettesGalleryScreen extends Screen {
  type Model = PalettesGalleryModel
  type Msg   = PalettesGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.PalettesId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.palettes)(
      PalettesGalleryMsg.Loaded.apply,
      _ => PalettesGalleryMsg.Loaded(Nil),
      (_, _) => PalettesGalleryMsg.Loaded(Nil)
    )
    (PalettesGalleryModel(None, None), cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PalettesGalleryMsg.Loaded(list) =>
      (model.copy(list = Some(list)), Cmd.None)
    case PalettesGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.PaletteId, None)))
    case PalettesGalleryMsg.Edit(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.PaletteId, Some(ScreenOutput.EditPalette(stored)))))
    case PalettesGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case PalettesGalleryMsg.ConfirmDelete(id) =>
      model.list match {
        case Some(list) =>
          val newList = list.filterNot(_.id == id)
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.palettes, newList)(
            _ => PalettesGalleryMsg.CancelDelete,
            (_, _) => PalettesGalleryMsg.CancelDelete
          )
          (model.copy(list = Some(newList), pendingDeleteId = None), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case PalettesGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case PalettesGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    model.list match {
      case None =>
        div(style := container)(p(text("Loading…")))
      case Some(list) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Palettes")),
            button(style := "padding: 6px 12px; cursor: pointer;", onClick(PalettesGalleryMsg.Back))(
              text("← Overview")
            )
          ),
          if (list.isEmpty)
            emptyState("Create Palette", PalettesGalleryMsg.CreateNew, "No palettes yet.")
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(PalettesGalleryMsg.CreateNew)
              )(text("+ Create Palette")))*
            )
        )
    }
  }

  private def entryCard(item: StoredPalette, confirmingDelete: Boolean): Html[Msg] =
    div(
      style := "display: flex; align-items: center; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(
        style := "display: flex; flex-wrap: wrap; gap: 2px; flex-shrink: 0;"
      )(
        item.colors.take(16).toList.map(c =>
          div(
            style := s"width: 20px; height: 20px; border-radius: 2px; border: 1px solid #999; background: ${c.toHex};"
          )()
        )*
      ),
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(s"${item.colors.length} color(s)")
        ),
        if (confirmingDelete)
          div(style := "margin-top: 0.5rem; padding: 6px 0;")(
            span(style := "font-size: 0.875rem; color: #b71c1c; margin-right: 8px;")(text(s"Delete \"${item.name}\"?")),
            button(
              style := "padding: 4px 10px; margin-right: 6px; cursor: pointer; background: #b71c1c; color: #fff; border: none; border-radius: 4px; font-size: 0.875rem;",
              onClick(PalettesGalleryMsg.ConfirmDelete(item.id))
            )(text("Yes")),
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #999; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(PalettesGalleryMsg.CancelDelete)
            )(text("Cancel"))
          )
        else
          div(style := "margin-top: 0.5rem; display: flex; gap: 6px;")(
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #555; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(PalettesGalleryMsg.Edit(item))
            )(text("Edit")),
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #b71c1c; color: #b71c1c; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(PalettesGalleryMsg.Delete(item))
            )(text("Delete"))
          )
      )
    )

  private def emptyState(createLabel: String, createMsg: Msg, emptyText: String): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text(emptyText)),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(s"+ $createLabel"))
    )
}

final case class PalettesGalleryModel(
    list: Option[List[StoredPalette]],
    pendingDeleteId: Option[String]
)

enum PalettesGalleryMsg:
  case Loaded(list: List[StoredPalette])
  case CreateNew
  case Edit(stored: StoredPalette)
  case Delete(stored: StoredPalette)
  case ConfirmDelete(id: String)
  case CancelDelete
  case Back
