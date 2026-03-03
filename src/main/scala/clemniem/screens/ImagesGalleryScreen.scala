package clemniem.screens

import cats.effect.IO
import clemniem.{Color, Screen, ScreenId, ScreenOutput, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, CmdUtils, Loadable, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved images. Empty state: "Upload". */
object ImagesGalleryScreen extends Screen {
  type Model = Gallery.State[StoredImage]
  type Msg   = ImagesGalleryMsg

  val screenId: ScreenId = ScreenId.ImagesId

  private val previewWidth  = CanvasUtils.galleryPreviewWidth
  private val previewHeight = CanvasUtils.galleryPreviewHeight

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val cmd = Gallery.loadCmd(StorageKeys.images, ImagesGalleryMsg.Loaded.apply, (msg, _) => ImagesGalleryMsg.LoadFailed(msg))
    (Gallery.initState, cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImagesGalleryMsg.Loaded(list) =>
      val next = Gallery.onLoaded(model, list, GalleryLayout.defaultPageSize)
      val cmd =
        if (list.isEmpty) Cmd.None
        else CmdUtils.fireAndForget(CanvasUtils.runAfterFrames(3)(drawPreviewsIO(list)), ImagesGalleryMsg.NoOp, _ => ImagesGalleryMsg.NoOp)
      (next, cmd)
    case ImagesGalleryMsg.CreateNew =>
      (model, navCmd(ScreenId.ImageUploadId, None))
    case ImagesGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case ImagesGalleryMsg.DrawPreview(stored) =>
      (model, CmdUtils.fireAndForget(drawPreview(stored), ImagesGalleryMsg.NoOp, _ => ImagesGalleryMsg.NoOp))
    case ImagesGalleryMsg.Delete(stored) =>
      (Gallery.onRequestDelete(model, stored.id), Cmd.None)
    case ImagesGalleryMsg.ConfirmDelete(id) =>
      Gallery.onConfirmDelete(model, id, StorageKeys.images, GalleryLayout.defaultPageSize, ImagesGalleryMsg.CancelDelete)
    case ImagesGalleryMsg.CancelDelete =>
      (Gallery.onCancelDelete(model), Cmd.None)
    case ImagesGalleryMsg.PreviousPage =>
      val next = Gallery.onPreviousPage(model)
      (next, redrawCurrentPageCmd(next))
    case ImagesGalleryMsg.NextPage =>
      val next = Gallery.onNextPage(model, GalleryLayout.defaultPageSize)
      (next, redrawCurrentPageCmd(next))
    case ImagesGalleryMsg.LoadFailed(error) =>
      (Gallery.onLoadFailed(model, error), Cmd.None)
    case ImagesGalleryMsg.ClearData =>
      val cmd = LocalStorageUtils.remove(StorageKeys.images)(
        _ => ImagesGalleryMsg.Retry,
        (_, _) => ImagesGalleryMsg.Retry
      )
      (Gallery.initState, cmd)
    case ImagesGalleryMsg.Retry =>
      val cmd = Gallery.loadCmd(StorageKeys.images, ImagesGalleryMsg.Loaded.apply, (msg, _) => ImagesGalleryMsg.LoadFailed(msg))
      (Gallery.initState, cmd)
    case ImagesGalleryMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    Gallery.view(
      screenId.title,
      model,
      GalleryLayout.defaultPageSize,
      shortHeader = true,
      ImagesGalleryMsg.Back,
      navMsg(ScreenFlow.nextInOverviewOrder(screenId), None),
      ImagesGalleryMsg.PreviousPage,
      ImagesGalleryMsg.NextPage,
      ImagesGalleryMsg.ClearData,
      ImagesGalleryMsg.Retry,
      GalleryEmptyState("No images yet.", "Upload", ImagesGalleryMsg.CreateNew),
      button(`class` := NesCss.btnPrimary, onClick(ImagesGalleryMsg.CreateNew))(text("Upload")),
      entryCard
    )

  private def entryCard(item: StoredImage, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.pixelPic.width}\u00d7${item.pixelPic.height} px \u00b7 ${item.pixelPic.paletteLookup.size} colors")
        ),
        paletteRow(item),
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          ImagesGalleryMsg.ConfirmDelete.apply,
          ImagesGalleryMsg.CancelDelete,
          button(`class` := NesCss.btnError, onClick(ImagesGalleryMsg.Delete(item)))(text("Delete"))
        )
      ),
      div(`class` := "gallery-card-preview")(
        div(`class` := "gallery-preview-wrap")(
          PixelPreviewBox(
            s"image-preview-${item.id}",
            previewWidth,
            previewHeight,
            Some(ImagesGalleryMsg.DrawPreview(item))
          )
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
      button(
        `class` := s"${NesCss.btn} palette-button-inline",
        onClick(navMsg(ScreenId.PaletteId, Some(output))))(
        PaletteStripView.swatches(colors)*
      )
    )
  }

  private def redrawCurrentPageCmd(model: Model): Cmd[IO, Msg] =
    model.items match {
      case Loadable.Loaded(list) if list.nonEmpty =>
        val start = (model.currentPage - 1) * GalleryLayout.defaultPageSize
        val slice = list.slice(start, start + GalleryLayout.defaultPageSize)
        CmdUtils.fireAndForget(CanvasUtils.runAfterFrames(3)(drawPreviewsIO(slice)), ImagesGalleryMsg.NoOp, _ => ImagesGalleryMsg.NoOp)
      case _ => Cmd.None
    }

  private def drawPreviewsIO(items: List[StoredImage]): IO[Unit] =
    items.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))

  private def drawPreview(stored: StoredImage): IO[Unit] =
    CanvasUtils.drawGalleryPreview(s"image-preview-${stored.id}")((_: Canvas, ctx: CanvasRenderingContext2D) => {
      ctx.clearRect(0, 0, previewWidth, previewHeight)
      val pic = stored.pixelPic
      if (pic.width > 0 && pic.height > 0) {
        val fit = CanvasUtils.scaleToFit(pic.width, pic.height, previewWidth, previewHeight, Double.MaxValue)
        CanvasUtils.drawPixelPic(ctx, pic, fit.width, fit.height, fit.offsetX, fit.offsetY)
      }
    })
}

enum ImagesGalleryMsg {
  case Loaded(list: List[StoredImage])
  case LoadFailed(error: String)
  case ClearData
  case Retry
  case CreateNew
  case DrawPreview(stored: StoredImage)
  case Delete(stored: StoredImage)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
  case NoOp
}
