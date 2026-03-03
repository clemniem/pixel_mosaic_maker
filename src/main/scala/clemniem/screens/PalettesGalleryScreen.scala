package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.{
  Color,
  PixelPic,
  PixelPicService,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredPalette
}
import clemniem.common.{CmdUtils, Loadable, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.*

/** Gallery of saved palettes. Empty state: "+ Create Palette". "From image" creates a palette from an image file. */
object PalettesGalleryScreen extends Screen {
  type Model = Gallery.State[StoredPalette]
  type Msg   = PalettesGalleryMsg

  val screenId: ScreenId = ScreenId.PalettesId

  private val pageSize: Int = 4

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val cmd = Gallery.loadCmd(StorageKeys.palettes, PalettesGalleryMsg.Loaded.apply, (msg, _) => PalettesGalleryMsg.LoadFailed(msg))
    (Gallery.initState, cmd)
  }

  private def baseNameFromFileName(fileName: String): String = {
    val i = fileName.lastIndexOf('.')
    if (i <= 0) fileName else fileName.substring(0, i)
  }

  /** Creates a temporary file input, opens the picker, and completes with the result. Input is never in the VDom. */
  private def openFilePickerForPalette(): IO[PalettesGalleryMsg] =
    IO.async_[PalettesGalleryMsg] { cb =>
      val input = dom.document.createElement("input").asInstanceOf[Input]
      input.`type` = "file"
      input.accept = "image/*"
      input.style.display = "none"
      val _               = dom.document.body.appendChild(input)
      def cleanup(): Unit = Option(input.parentNode).foreach(_.removeChild(input))
      input.addEventListener(
        "change",
        (_: dom.Event) => {
          val file = Option(input.files(0))
          input.value = "" // only allowed value for file input
          cleanup()
          file.foreach { f =>
            val fileName = Option(f.name).filter(_.nonEmpty)
            PixelPicService.loadPixelImageFromFile(f).unsafeRunAsync {
              case Right(opt) =>
                val msg =
                  opt.fold[PalettesGalleryMsg](PalettesGalleryMsg.PaletteFromImageError("Could not decode image"))(
                    pic => PalettesGalleryMsg.PaletteFromImageDecoded(pic, fileName))
                cb(Right(msg))
              case Left(err) =>
                cb(Right(PalettesGalleryMsg.PaletteFromImageError(err.getMessage)))
            }
          }
        }
      )
      input.click()
    }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PalettesGalleryMsg.Loaded(list) =>
      (Gallery.onLoaded(model, list, pageSize), Cmd.None)
    case PalettesGalleryMsg.CreateNew =>
      (model, navCmd(ScreenId.PaletteId, None))
    case PalettesGalleryMsg.Edit(stored) =>
      (model, navCmd(ScreenId.PaletteId, Some(ScreenOutput.EditPalette(stored))))
    case PalettesGalleryMsg.Copy(stored) =>
      model.items match {
        case Loadable.Loaded(list) =>
          val newId   = LocalStorageUtils.newId("palette")
          val copy    = StoredPalette(id = newId, name = stored.name + " copy", colors = stored.colors)
          val idx     = list.indexWhere(_.id == stored.id)
          val newList = if (idx >= 0) list.patch(idx + 1, List(copy), 0) else list :+ copy
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.palettes, newList)(
            _ => PalettesGalleryMsg.CopySaved,
            (_, _) => PalettesGalleryMsg.CopyFailed
          )
          (model.copy(items = Loadable.Loaded(newList)), saveCmd)
        case _ => (model, Cmd.None)
      }
    case PalettesGalleryMsg.CopySaved  => (model, Cmd.None)
    case PalettesGalleryMsg.CopyFailed => (model, Cmd.None)
    case PalettesGalleryMsg.Delete(stored) =>
      (Gallery.onRequestDelete(model, stored.id), Cmd.None)
    case PalettesGalleryMsg.ConfirmDelete(id) =>
      Gallery.onConfirmDelete(model, id, StorageKeys.palettes, pageSize, PalettesGalleryMsg.CancelDelete)
    case PalettesGalleryMsg.CancelDelete =>
      (Gallery.onCancelDelete(model), Cmd.None)
    case PalettesGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case PalettesGalleryMsg.PaletteFromImageDecoded(pic, fileName) =>
      val name   = fileName.map(baseNameFromFileName).filter(_.nonEmpty).getOrElse("Unnamed palette")
      val colors = pic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toVector
      val id     = LocalStorageUtils.newId("palette")
      val stored = StoredPalette(id = id, name = name, colors = colors)
      model.items match {
        case Loadable.Loaded(list) =>
          val newList = list :+ stored
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.palettes, newList)(
            _ => PalettesGalleryMsg.PaletteFromImageSaved,
            (_, _) => PalettesGalleryMsg.PaletteFromImageError("Failed to save palette")
          )
          (model.copy(items = Loadable.Loaded(newList)), saveCmd)
        case _ =>
          (model, Cmd.None)
      }
    case PalettesGalleryMsg.PaletteFromImageError(_) =>
      (model, Cmd.None)
    case PalettesGalleryMsg.PaletteFromImageSaved =>
      (model, Cmd.None)
    case PalettesGalleryMsg.RequestPaletteFromImage =>
      (
        model,
        CmdUtils.run(
          openFilePickerForPalette(),
          identity[PalettesGalleryMsg],
          e => PalettesGalleryMsg.PaletteFromImageError(e.getMessage)))
    case PalettesGalleryMsg.PreviousPage =>
      (Gallery.onPreviousPage(model), Cmd.None)
    case PalettesGalleryMsg.NextPage =>
      (Gallery.onNextPage(model, pageSize), Cmd.None)
    case PalettesGalleryMsg.LoadFailed(error) =>
      (Gallery.onLoadFailed(model, error), Cmd.None)
    case PalettesGalleryMsg.ClearData =>
      val cmd = LocalStorageUtils.remove(StorageKeys.palettes)(
        _ => PalettesGalleryMsg.Retry,
        (_, _) => PalettesGalleryMsg.Retry
      )
      (Gallery.initState, cmd)
    case PalettesGalleryMsg.Retry =>
      val cmd = Gallery.loadCmd(StorageKeys.palettes, PalettesGalleryMsg.Loaded.apply, (msg, _) => PalettesGalleryMsg.LoadFailed(msg))
      (Gallery.initState, cmd)
  }

  def view(model: Model): Html[Msg] = {
    val actionRow = div(`class` := "flex-row")(
      button(`class` := NesCss.btnPrimary, onClick(PalettesGalleryMsg.CreateNew))(text("+ Create Palette")),
      button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.RequestPaletteFromImage))(text("From image"))
    )
    val emptyContent = div(`class` := GalleryLayout.galleryListClass)(
      actionRow,
      GalleryEmptyState("No palettes yet.", "+ Create Palette", PalettesGalleryMsg.CreateNew)
    )
    Gallery.view(
      screenId.title,
      model,
      pageSize,
      shortHeader = false,
      PalettesGalleryMsg.Back,
      navMsg(ScreenFlow.nextInOverviewOrder(screenId), None),
      PalettesGalleryMsg.PreviousPage,
      PalettesGalleryMsg.NextPage,
      PalettesGalleryMsg.ClearData,
      PalettesGalleryMsg.Retry,
      emptyContent,
      actionRow,
      entryCard
    )
  }

  private def entryCard(item: StoredPalette, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card gallery-card--palette")(
      div(`class` := "gallery-card-header")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(s" \u00b7 ${item.colors.length} colors"))
      ),
      div(`class` := "gallery-card-row2")(
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          PalettesGalleryMsg.ConfirmDelete.apply,
          PalettesGalleryMsg.CancelDelete,
          button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.Edit(item)))(text("Edit")),
          button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.Copy(item)))(text("Copy")),
          button(`class` := NesCss.btnError, onClick(PalettesGalleryMsg.Delete(item)))(text("Delete"))
        ),
        div(`class` := "gallery-card-preview")(
          PaletteStripView.previewInline(item.colors.toList)
        )
      )
    )
}

enum PalettesGalleryMsg {
  case Loaded(list: List[StoredPalette])
  case LoadFailed(error: String)
  case ClearData
  case Retry
  case CreateNew
  case RequestPaletteFromImage
  case PaletteFromImageDecoded(pic: PixelPic, fileName: Option[String])
  case PaletteFromImageError(message: String)
  case PaletteFromImageSaved
  case Edit(stored: StoredPalette)
  case Copy(stored: StoredPalette)
  case CopySaved
  case CopyFailed
  case Delete(stored: StoredPalette)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
}
