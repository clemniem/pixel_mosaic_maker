package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredPalette}
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
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
    val root = s"${NesCss.container} ${NesCss.containerRounded} screen-container"
    model.list match {
      case None =>
        div(`class` := root)(p(`class` := NesCss.text)(text("Loading…")))
      case Some(list) =>
        div(`class` := root)(
          div(`class` := "screen-header")(
            h1(`class` := "screen-title")(text("Palettes")),
            button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.Back))(text("← Overview"))
          ),
          if (list.isEmpty)
            GalleryEmptyState("No palettes yet.", "+ Create Palette", PalettesGalleryMsg.CreateNew)
          else
            div(`class` := "flex-col")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+
                button(`class` := NesCss.btnPrimary, onClick(PalettesGalleryMsg.CreateNew))(text("+ Create Palette")))*
            )
        )
    }
  }

  private def entryCard(item: StoredPalette, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "palette-strip")(
        item.colors.take(16).toList.map(c =>
          div(`class` := "palette-strip-swatch", style := s"background: ${c.toHex};")()
        )*
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(s"${item.colors.length} color(s)")),
        if (confirmingDelete)
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text nes-text")(text(s"Delete \"${item.name}\"?")),
            button(`class` := NesCss.btnError, style := "margin-right: 6px;", onClick(PalettesGalleryMsg.ConfirmDelete(item.id)))(text("Yes")),
            button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.Edit(item)))(text("Edit")),
            button(`class` := NesCss.btnError, onClick(PalettesGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
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
