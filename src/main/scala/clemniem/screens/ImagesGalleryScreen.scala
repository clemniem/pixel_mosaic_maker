package clemniem.screens

import cats.effect.IO
import clemniem.{Color, NavigateNext, PixelPic, Screen, ScreenId, ScreenOutput, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved images. Empty state: "Upload". */
object ImagesGalleryScreen extends Screen {
  type Model = ImagesGalleryModel
  type Msg   = ImagesGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ImagesId

  private val previewWidth  = 120
  private val previewHeight = 80

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.images)(
      ImagesGalleryMsg.Loaded.apply,
      _ => ImagesGalleryMsg.Loaded(Nil),
      (_, _) => ImagesGalleryMsg.Loaded(Nil)
    )
    (ImagesGalleryModel(None, None), cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImagesGalleryMsg.Loaded(list) =>
      val drawPreviews =
        if (list.isEmpty) Cmd.None
        else
          Cmd.SideEffect(
            CanvasUtils.runAfterFrames(3)(
              list.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
            )
          )
      (model.copy(list = Some(list)), drawPreviews)
    case ImagesGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ImageUploadId, None)))
    case ImagesGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case ImagesGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
    case ImagesGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case ImagesGalleryMsg.ConfirmDelete(id) =>
      model.list match {
        case Some(list) =>
          val newList = list.filterNot(_.id == id)
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.images, newList)(
            _ => ImagesGalleryMsg.CancelDelete,
            (_, _) => ImagesGalleryMsg.CancelDelete
          )
          (model.copy(list = Some(newList), pendingDeleteId = None), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case ImagesGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    model.list match {
      case None =>
        div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container")(
          p(`class` := NesCss.text)(text("Loading…"))
        )
      case Some(list) =>
        div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container")(
          div(`class` := "screen-header screen-header--short")(
            h1(`class` := "screen-title")(text("Images")),
            button(`class` := NesCss.btn, onClick(ImagesGalleryMsg.Back))(text("← Overview"))
          ),
          if (list.isEmpty)
            GalleryEmptyState("No images yet.", "Upload", ImagesGalleryMsg.CreateNew)
          else
            div(`class` := "flex-col flex-col--gap-tight")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+
                button(`class` := NesCss.btnPrimary, style := "margin-top: 0.5rem;", onClick(ImagesGalleryMsg.CreateNew))(text("Upload")))*
            )
        )
    }

  private def entryCard(item: StoredImage, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.pixelPic.width}×${item.pixelPic.height} px · ${item.pixelPic.paletteLookup.size} colors")
        ),
        paletteRow(item),
        if (confirmingDelete)
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text nes-text")(text(s"Delete \"${item.name}\"?")),
            button(`class` := NesCss.btnError, style := "margin-right: 6px;", onClick(ImagesGalleryMsg.ConfirmDelete(item.id)))(text("Yes")),
            button(`class` := NesCss.btn, onClick(ImagesGalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := NesCss.btnError, onClick(ImagesGalleryMsg.Delete(item)))(text("Delete"))
          )
      ),
      div(`class` := "gallery-card-preview")(
        PixelPreviewBox(
          s"image-preview-${item.id}",
          previewWidth,
          previewHeight,
          Some(ImagesGalleryMsg.DrawPreview(item))
        )
      )
    )

  private def paletteRow(item: StoredImage): Html[Msg] = {
    val colors = item.pixelPic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toList
    val output = ScreenOutput.NewPaletteFromImage(
      name = item.name + " palette",
      colors = item.pixelPic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toVector
    )
    div(style := "margin-top: 0.35rem;", title := "Click to save as palette")(
      button(`class` := s"${NesCss.btn} palette-button-inline", onClick(NavigateNext(ScreenId.PaletteId, Some(output))))(
        PaletteStripView.swatches(colors)*
      )
    )
  }

  private def drawPreview(stored: StoredImage): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"image-preview-${stored.id}",
      framesToWait = 2,
      maxRetries = 100,
      delayMs = 3
    )((canvas: Canvas, ctx: CanvasRenderingContext2D) => drawPixelPicScaled(canvas, ctx, stored.pixelPic))

  private def drawPixelPicScaled(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic): Unit = {
    if (pic.width > 0 && pic.height > 0) {
      val scale = (previewWidth.toDouble / pic.width).min(previewHeight.toDouble / pic.height)
      val cw = (pic.width * scale).toInt.max(1)
      val ch = (pic.height * scale).toInt.max(1)
      canvas.width = cw
      canvas.height = ch
      ctx.clearRect(0, 0, cw, ch)
      CanvasUtils.drawPixelPic(canvas, ctx, pic, cw, ch)
    } else {
      canvas.width = previewWidth
      canvas.height = previewHeight
      ctx.clearRect(0, 0, previewWidth, previewHeight)
    }
  }

}

final case class ImagesGalleryModel(
    list: Option[List[StoredImage]],
    pendingDeleteId: Option[String]
)

enum ImagesGalleryMsg:
  case Loaded(list: List[StoredImage])
  case CreateNew
  case DrawPreview(stored: StoredImage)
  case Delete(stored: StoredImage)
  case ConfirmDelete(id: String)
  case CancelDelete
  case Back
