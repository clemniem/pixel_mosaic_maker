package clemniem.screens

import cats.effect.IO
import clemniem.{Screen, ScreenId, ScreenOutput, StorageKeys, StoredLayout}
import clemniem.common.{CanvasUtils, CmdUtils, Loadable, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved layouts. Empty state: "+ New layout". */
object LayoutGalleryScreen extends Screen {
  type Model = Gallery.State[StoredLayout]
  type Msg   = LayoutGalleryMsg

  val screenId: ScreenId = ScreenId.LayoutsId

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val cmd = Gallery.loadCmd(StorageKeys.layouts, LayoutGalleryMsg.Loaded.apply, (msg, _) => LayoutGalleryMsg.LoadFailed(msg))
    (Gallery.initState, cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LayoutGalleryMsg.Loaded(list) =>
      val next = Gallery.onLoaded(model, list, GalleryLayout.defaultPageSize)
      val cmd =
        if (list.isEmpty) Cmd.None
        else CmdUtils.fireAndForget(CanvasUtils.runAfterFrames(3)(drawPreviewsIO(list)), LayoutGalleryMsg.NoOp, _ => LayoutGalleryMsg.NoOp)
      (next, cmd)
    case LayoutGalleryMsg.Edit(stored) =>
      (model, navCmd(ScreenId.LayoutId, Some(ScreenOutput.EditLayout(stored))))
    case LayoutGalleryMsg.Delete(stored) =>
      (Gallery.onRequestDelete(model, stored.id), Cmd.None)
    case LayoutGalleryMsg.ConfirmDelete(id) =>
      Gallery.onConfirmDelete(model, id, StorageKeys.layouts, GalleryLayout.defaultPageSize, LayoutGalleryMsg.CancelDelete)
    case LayoutGalleryMsg.CancelDelete =>
      (Gallery.onCancelDelete(model), Cmd.None)
    case LayoutGalleryMsg.CreateNew =>
      (model, navCmd(ScreenId.LayoutId, None))
    case LayoutGalleryMsg.PreviousPage =>
      val next = Gallery.onPreviousPage(model)
      val cmd = redrawCurrentPageCmd(next)
      (next, cmd)
    case LayoutGalleryMsg.NextPage =>
      val next = Gallery.onNextPage(model, GalleryLayout.defaultPageSize)
      val cmd = redrawCurrentPageCmd(next)
      (next, cmd)
    case LayoutGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case LayoutGalleryMsg.DrawPreview(stored) =>
      (model, CmdUtils.fireAndForget(drawPreview(stored), LayoutGalleryMsg.NoOp, _ => LayoutGalleryMsg.NoOp))
    case LayoutGalleryMsg.LoadFailed(error) =>
      (Gallery.onLoadFailed(model, error), Cmd.None)
    case LayoutGalleryMsg.ClearData =>
      val cmd = LocalStorageUtils.remove(StorageKeys.layouts)(
        _ => LayoutGalleryMsg.Retry,
        (_, _) => LayoutGalleryMsg.Retry
      )
      (Gallery.initState, cmd)
    case LayoutGalleryMsg.Retry =>
      val cmd = Gallery.loadCmd(StorageKeys.layouts, LayoutGalleryMsg.Loaded.apply, (msg, _) => LayoutGalleryMsg.LoadFailed(msg))
      (Gallery.initState, cmd)
    case LayoutGalleryMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    Gallery.view(
      screenId.title,
      model,
      GalleryLayout.defaultPageSize,
      shortHeader = true,
      LayoutGalleryMsg.Back,
      navMsg(ScreenFlow.nextInOverviewOrder(screenId), None),
      LayoutGalleryMsg.PreviousPage,
      LayoutGalleryMsg.NextPage,
      LayoutGalleryMsg.ClearData,
      LayoutGalleryMsg.Retry,
      GalleryEmptyState("No layouts yet.", "+ New layout", LayoutGalleryMsg.CreateNew),
      button(`class` := NesCss.btnPrimary, onClick(LayoutGalleryMsg.CreateNew))(text("+ New layout")),
      entryCard
    )

  private def entryCard(item: StoredLayout, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-preview-wrap")(
        div(onLoad(LayoutGalleryMsg.DrawPreview(item)))(
          canvas(
            id      := s"grid-preview-${item.id}",
            width   := previewWidth,
            height  := previewHeight,
            `class` := "gallery-preview-canvas"
          )()
        )
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.width}\u00d7${item.config.height} \u00b7 ${item.config.parts.length} section(s)")
        ),
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          LayoutGalleryMsg.ConfirmDelete.apply,
          LayoutGalleryMsg.CancelDelete,
          button(`class` := NesCss.btn, onClick(LayoutGalleryMsg.Edit(item)))(text("Edit")),
          button(`class` := NesCss.btnError, onClick(LayoutGalleryMsg.Delete(item)))(text("Delete"))
        )
      )
    )

  private val previewWidth  = CanvasUtils.galleryPreviewWidth
  private val previewHeight = CanvasUtils.galleryPreviewHeight

  private def redrawCurrentPageCmd(model: Model): Cmd[IO, Msg] =
    model.items match {
      case Loadable.Loaded(list) if list.nonEmpty =>
        val start = (model.currentPage - 1) * GalleryLayout.defaultPageSize
        val slice = list.slice(start, start + GalleryLayout.defaultPageSize)
        CmdUtils.fireAndForget(CanvasUtils.runAfterFrames(3)(drawPreviewsIO(slice)), LayoutGalleryMsg.NoOp, _ => LayoutGalleryMsg.NoOp)
      case _ => Cmd.None
    }

  private def drawPreviewsIO(items: List[StoredLayout]): IO[Unit] =
    items.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))

  private def drawPreview(stored: StoredLayout): IO[Unit] =
    CanvasUtils.drawGalleryPreview(s"grid-preview-${stored.id}")((_: Canvas, ctx: CanvasRenderingContext2D) =>
      renderers.LayoutRenderer.drawGalleryPreview(ctx, stored.config, previewWidth, previewHeight))
}

enum LayoutGalleryMsg {
  case Loaded(list: List[StoredLayout])
  case LoadFailed(error: String)
  case ClearData
  case Retry
  case Edit(stored: StoredLayout)
  case Delete(stored: StoredLayout)
  case ConfirmDelete(id: String)
  case CancelDelete
  case DrawPreview(stored: StoredLayout)
  case CreateNew
  case PreviousPage
  case NextPage
  case Back
  case NoOp
}
