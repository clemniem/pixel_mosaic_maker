package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.{Color, NavigateNext, PixelPic, Screen, ScreenId, ScreenOutput, StorageKeys, StoredPalette}
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Gallery of saved palettes. Empty state: "+ Create Palette". "From image" creates a palette from an image file. */
object PalettesGalleryScreen extends Screen {
  type Model = PalettesGalleryModel
  type Msg   = PalettesGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.PalettesId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val loadCmd = LocalStorageUtils.loadList(StorageKeys.palettes)(
      PalettesGalleryMsg.Loaded.apply,
      _ => PalettesGalleryMsg.Loaded(Nil),
      (_, _) => PalettesGalleryMsg.Loaded(Nil)
    )
    (PalettesGalleryModel(None, None), loadCmd)
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
      val _ = dom.document.body.appendChild(input)
      def cleanup(): Unit = Option(input.parentNode).foreach(_.removeChild(input))
      input.addEventListener("change", (_: dom.Event) => {
        val file = Option(input.files(0))
        input.value = "" // only allowed value for file input
        cleanup()
        file.foreach { f =>
          val fileName = Option(f.name).filter(_.nonEmpty)
          PixelPic.loadPixelImageFromFile(f).unsafeRunAsync {
            case Right(opt) =>
              val msg = opt.fold[PalettesGalleryMsg](PalettesGalleryMsg.PaletteFromImageError("Could not decode image"))(pic =>
                PalettesGalleryMsg.PaletteFromImageDecoded(pic, fileName))
              cb(Right(msg))
            case Left(err) =>
              cb(Right(PalettesGalleryMsg.PaletteFromImageError(err.getMessage)))
          }
        }
      })
      input.click()
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
    case PalettesGalleryMsg.PaletteFromImageDecoded(pic, fileName) =>
      val name = fileName.map(baseNameFromFileName).filter(_.nonEmpty).getOrElse("Unnamed palette")
      val colors = pic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toVector
      val id    = "palette-" + js.Date.now().toLong
      val stored = StoredPalette(id = id, name = name, colors = colors)
      model.list match {
        case Some(list) =>
          val newList = list :+ stored
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.palettes, newList)(
            _ => PalettesGalleryMsg.PaletteFromImageSaved,
            (_, _) => PalettesGalleryMsg.PaletteFromImageError("Failed to save palette")
          )
          (model.copy(list = Some(newList)), saveCmd)
        case None =>
          (model, Cmd.None)
      }
    case PalettesGalleryMsg.PaletteFromImageError(_) =>
      (model, Cmd.None)
    case PalettesGalleryMsg.PaletteFromImageSaved =>
      (model, Cmd.None)
    case PalettesGalleryMsg.RequestPaletteFromImage =>
      (model, Cmd.Run(openFilePickerForPalette(), (m: PalettesGalleryMsg) => m))
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
            div(`class` := "flex-col")(
              GalleryEmptyState("No palettes yet.", "+ Create Palette", PalettesGalleryMsg.CreateNew),
              div(`class` := "flex-row", style := "margin-top: 0.5rem;")(
                button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.RequestPaletteFromImage))(text("From image"))
              )
            )
          else
            div(`class` := "flex-col")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+
                div(`class` := "flex-row", style := "margin-top: 0.5rem;")(
                  button(`class` := NesCss.btnPrimary, onClick(PalettesGalleryMsg.CreateNew))(text("+ Create Palette")),
                  button(`class` := NesCss.btn, onClick(PalettesGalleryMsg.RequestPaletteFromImage))(text("From image"))
                ))*
            )
        )
    }
  }

  private def entryCard(item: StoredPalette, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(s"${item.colors.length} colors")),
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
      ),
      div(`class` := "gallery-card-preview")(
        PaletteStripView.previewInline(item.colors.toList)
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
  case RequestPaletteFromImage
  case PaletteFromImageDecoded(pic: PixelPic, fileName: Option[String])
  case PaletteFromImageError(message: String)
  case PaletteFromImageSaved
  case Edit(stored: StoredPalette)
  case Delete(stored: StoredPalette)
  case ConfirmDelete(id: String)
  case CancelDelete
  case Back
