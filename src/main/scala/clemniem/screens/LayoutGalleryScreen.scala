package clemniem.screens

import cats.effect.IO
import clemniem.{Layout, NavigateNext, Screen, ScreenId, ScreenOutput, StoredLayout, StorageKeys}
import clemniem.common.CanvasUtils
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved layouts. Empty state: "+ New layout". */
object LayoutGalleryScreen extends Screen {
  type Model = LayoutGalleryModel
  type Msg   = LayoutGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LayoutsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.layouts)(
      LayoutGalleryMsg.Loaded.apply,
      _ => LayoutGalleryMsg.Loaded(Nil),
      (_, _) => LayoutGalleryMsg.Loaded(Nil)
    )
    (LayoutGalleryModel(None, None, currentPage = 1), cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LayoutGalleryMsg.Loaded(list) =>
      val drawPreviews =
        if (list.isEmpty) Cmd.None
        else
          Cmd.SideEffect(
            CanvasUtils.runAfterFrames(3)(
              list.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
            )
          )
      val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
      (model.copy(list = Some(list), currentPage = GalleryLayout.clampPage(model.currentPage, totalPages)), drawPreviews)
    case LayoutGalleryMsg.Edit(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.LayoutId, Some(ScreenOutput.EditLayout(stored)))))
    case LayoutGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case LayoutGalleryMsg.ConfirmDelete(id) =>
      val (newList, newPage, cmd) = LocalStorageUtils.confirmDelete(
        model.list, id, StorageKeys.layouts, GalleryLayout.defaultPageSize, model.currentPage, LayoutGalleryMsg.CancelDelete, _.id
      )
      (model.copy(list = newList, pendingDeleteId = None, currentPage = newPage), cmd)
    case LayoutGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case LayoutGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.LayoutId, None)))
    case LayoutGalleryMsg.PreviousPage =>
      val next = model.copy(currentPage = (model.currentPage - 1).max(1))
      val cmd  = model.list match {
        case Some(list) if list.nonEmpty =>
          Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
        case _ => Cmd.None
      }
      (next, cmd)
    case LayoutGalleryMsg.NextPage =>
      model.list match {
        case Some(list) =>
          val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
          val next       = model.copy(currentPage = (model.currentPage + 1).min(totalPages))
          val cmd = if (list.isEmpty) Cmd.None
          else Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
          (next, cmd)
        case None => (model, Cmd.None)
      }
    case LayoutGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case LayoutGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val backBtn = GalleryLayout.backButton(LayoutGalleryMsg.Back, "Overview")
    val nextBtn = GalleryLayout.nextButton(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None))
    model.list match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = true, Some(nextBtn))
      case Some(list) =>
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No layouts yet.", "+ New layout", LayoutGalleryMsg.CreateNew)
          else
            paginatedList(
              list,
              model.currentPage,
              button(`class` := NesCss.btnPrimary, onClick(LayoutGalleryMsg.CreateNew))(text("+ New layout")),
              item => entryCard(item, model.pendingDeleteId.contains(item.id))
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = true, Some(nextBtn))
    }
  }

  private def paginatedList(
      list: List[StoredLayout],
      currentPage: Int,
      addAction: Html[Msg],
      entryCard: StoredLayout => Html[Msg]
  ): Html[Msg] =
    GalleryLayout.paginatedListWith(
      list,
      currentPage,
      GalleryLayout.defaultPageSize,
      addAction,
      entryCard,
      LayoutGalleryMsg.PreviousPage,
      LayoutGalleryMsg.NextPage
    )

  private def entryCard(item: StoredLayout, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-preview-wrap")(
        div(onLoad(LayoutGalleryMsg.DrawPreview(item)))(
          canvas(
            id := s"grid-preview-${item.id}",
            width := previewWidth,
            height := previewHeight,
            `class` := "gallery-preview-canvas"
          )()
        )
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.width}×${item.config.height} · ${item.config.parts.length} section(s)")
        ),
        if (confirmingDelete)
          GalleryLayout.galleryDeleteConfirm(
            s"Delete \"${item.name}\"?",
            LayoutGalleryMsg.ConfirmDelete(item.id),
            LayoutGalleryMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
            button(`class` := NesCss.btn, onClick(LayoutGalleryMsg.Edit(item)))(text("Edit")),
            button(`class` := NesCss.btnError, onClick(LayoutGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )

  private val previewWidth  = CanvasUtils.galleryPreviewWidth
  private val previewHeight = CanvasUtils.galleryPreviewHeight

  private def drawPreviewsForCurrentPage(model: Model): IO[Unit] =
    model.list match {
      case Some(list) if list.nonEmpty =>
        val pageSize = GalleryLayout.defaultPageSize
        val start    = (model.currentPage - 1) * pageSize
        val slice    = list.slice(start, start + pageSize)
        slice.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
      case _ => IO.unit
    }

  private def drawPreview(stored: StoredLayout): IO[Unit] =
    CanvasUtils.drawGalleryPreview(s"grid-preview-${stored.id}")((_: Canvas, ctx: CanvasRenderingContext2D) => drawGridScaled(ctx, stored.config))

  /** NES.css-style grid preview: thick dark borders, checkerboard fills, pixel-art bevel. */
  private def drawGridScaled(ctx: CanvasRenderingContext2D, grid: Layout): Unit = {
    ctx.imageSmoothingEnabled = false
    ctx.clearRect(0, 0, previewWidth, previewHeight)
    if (grid.parts.nonEmpty && grid.width > 0 && grid.height > 0) {
      // Leave a small margin so the outer border doesn't clip against the canvas edge
      val margin = 4
      val fit = CanvasUtils.scaleToFit(grid.width, grid.height, previewWidth - margin * 2, previewHeight - margin * 2, Double.MaxValue)
      val ox  = fit.offsetX + margin
      val oy  = fit.offsetY + margin

      // --- cell fills (checkerboard) ---
      grid.parts.zipWithIndex.foreach { case (part, i) =>
        val cx = ox + (part.x * fit.scale).toInt
        val cy = oy + (part.y * fit.scale).toInt
        val cw = (part.width * fit.scale).toInt.max(1)
        val ch = (part.height * fit.scale).toInt.max(1)
        ctx.fillStyle = if (i % 2 == 0) "#d4d4d8" else "#e4e4e7"
        ctx.fillRect(cx, cy, cw, ch)
        // bottom-right shadow (1px dark)
        ctx.fillStyle = "rgba(0,0,0,0.18)"
        ctx.fillRect(cx + cw - 1, cy, 1, ch)
        ctx.fillRect(cx, cy + ch - 1, cw, 1)
        // top-left highlight (1px light)
        ctx.fillStyle = "rgba(255,255,255,0.45)"
        ctx.fillRect(cx, cy, cw, 1)
        ctx.fillRect(cx, cy, 1, ch)
      }

      // --- thick cell borders ---
      val borderW = (2.0 / fit.scale).max(1.5).min(3.0)
      ctx.strokeStyle = "#212529"
      ctx.lineWidth = borderW * fit.scale
      grid.parts.foreach { part =>
        val cx = ox + (part.x * fit.scale).toInt
        val cy = oy + (part.y * fit.scale).toInt
        val cw = (part.width * fit.scale).toInt.max(1)
        val ch = (part.height * fit.scale).toInt.max(1)
        ctx.strokeRect(cx, cy, cw, ch)
      }

      // --- dark outer border around the full grid ---
      val totalW = (grid.width * fit.scale).toInt.max(1)
      val totalH = (grid.height * fit.scale).toInt.max(1)
      ctx.lineWidth = (borderW * fit.scale * 1.25).min(4.0)
      ctx.strokeStyle = "#212529"
      ctx.strokeRect(ox, oy, totalW, totalH)
    }
  }

}

final case class LayoutGalleryModel(
    list: Option[List[StoredLayout]],
    pendingDeleteId: Option[String],
    currentPage: Int
)

enum LayoutGalleryMsg:
  case Loaded(list: List[StoredLayout])
  case Edit(stored: StoredLayout)
  case Delete(stored: StoredLayout)
  case ConfirmDelete(id: String)
  case CancelDelete
  case DrawPreview(stored: StoredLayout)
  case CreateNew
  case PreviousPage
  case NextPage
  case Back
