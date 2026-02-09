package clemniem.screens

import cats.effect.IO
import clemniem.{GridConfig, NavigateNext, Screen, ScreenId, ScreenOutput, StoredGridConfig, StorageKeys}
import clemniem.common.CanvasUtils
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved grid configs. Empty state: "+ Create GridConfig". */
object GridConfigGalleryScreen extends Screen {
  type Model = GridConfigGalleryModel
  type Msg   = GridConfigGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.GridConfigsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.gridConfigs)(
      GridConfigGalleryMsg.Loaded.apply,
      _ => GridConfigGalleryMsg.Loaded(Nil),
      (_, _) => GridConfigGalleryMsg.Loaded(Nil)
    )
    (GridConfigGalleryModel(None, None, currentPage = 1), cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case GridConfigGalleryMsg.Loaded(list) =>
      val drawPreviews =
        if (list.isEmpty) Cmd.None
        else
          Cmd.SideEffect(
            CanvasUtils.runAfterFrames(3)(
              list.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
            )
          )
      val maxPage = if (list.isEmpty) 1 else ((list.size - 1) / GalleryLayout.defaultPageSize) + 1
      (model.copy(list = Some(list), currentPage = model.currentPage.min(maxPage).max(1)), drawPreviews)
    case GridConfigGalleryMsg.Edit(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GridConfigId, Some(ScreenOutput.EditGridConfig(stored)))))
    case GridConfigGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case GridConfigGalleryMsg.ConfirmDelete(id) =>
      model.list match {
        case Some(list) =>
          val newList = list.filterNot(_.id == id)
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.gridConfigs, newList)(
            _ => GridConfigGalleryMsg.CancelDelete,
            (_, _) => GridConfigGalleryMsg.CancelDelete
          )
          val maxPage = if (newList.isEmpty) 1 else ((newList.size - 1) / GalleryLayout.defaultPageSize) + 1
          (model.copy(list = Some(newList), pendingDeleteId = None, currentPage = model.currentPage.min(maxPage).max(1)), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case GridConfigGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case GridConfigGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GridConfigId, None)))
    case GridConfigGalleryMsg.PreviousPage =>
      val next = model.copy(currentPage = (model.currentPage - 1).max(1))
      val cmd  = model.list match {
        case Some(list) if list.nonEmpty =>
          Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
        case _ => Cmd.None
      }
      (next, cmd)
    case GridConfigGalleryMsg.NextPage =>
      model.list match {
        case Some(list) =>
          val maxPage = if (list.isEmpty) 1 else ((list.size - 1) / GalleryLayout.defaultPageSize) + 1
          val next    = model.copy(currentPage = (model.currentPage + 1).min(maxPage))
          val cmd = if (list.isEmpty) Cmd.None
          else Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
          (next, cmd)
        case None => (model, Cmd.None)
      }
    case GridConfigGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case GridConfigGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val backBtn  = button(`class` := NesCss.btn, onClick(GridConfigGalleryMsg.Back))(GalleryLayout.backButtonLabel("←", "Overview"))
    val nextBtn  = button(`class` := NesCss.btn, onClick(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None)))(GalleryLayout.nextButtonLabel("Next", "→"))
    model.list match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = true, Some(nextBtn))
      case Some(list) =>
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No layouts yet.", "+ New layout", GridConfigGalleryMsg.CreateNew)
          else
            paginatedList(
              list,
              model.currentPage,
              button(`class` := NesCss.btnPrimary, onClick(GridConfigGalleryMsg.CreateNew))(text("+ New layout")),
              item => entryCard(item, model.pendingDeleteId.contains(item.id))
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = true, Some(nextBtn))
    }
  }

  private def paginatedList(
      list: List[StoredGridConfig],
      currentPage: Int,
      addAction: Html[Msg],
      entryCard: StoredGridConfig => Html[Msg]
  ): Html[Msg] = {
    val pageSize   = GalleryLayout.defaultPageSize
    val totalPages = if (list.isEmpty) 1 else ((list.size - 1) / pageSize) + 1
    val page       = currentPage.min(totalPages).max(1)
    val start      = (page - 1) * pageSize
    val slice      = list.slice(start, start + pageSize)
    GalleryLayout.listWithAddActionAndPagination(
      addAction,
      slice.map(entryCard),
      page,
      totalPages,
      GridConfigGalleryMsg.PreviousPage,
      GridConfigGalleryMsg.NextPage
    )
  }

  private def entryCard(item: StoredGridConfig, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(onLoad(GridConfigGalleryMsg.DrawPreview(item)))(
        canvas(
          id := s"grid-preview-${item.id}",
          width := previewWidth,
          height := previewHeight,
          `class` := "gallery-preview-canvas"
        )()
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.width}×${item.config.height} · ${item.config.parts.length} plate(s)")
        ),
        if (confirmingDelete)
          GalleryLayout.galleryDeleteConfirm(
            s"Delete \"${item.name}\"?",
            GridConfigGalleryMsg.ConfirmDelete(item.id),
            GridConfigGalleryMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
            button(`class` := NesCss.btn, onClick(GridConfigGalleryMsg.Edit(item)))(text("Edit")),
            button(`class` := NesCss.btnError, onClick(GridConfigGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )

  private val previewWidth  = 120
  private val previewHeight = 80

  private def drawPreviewsForCurrentPage(model: Model): IO[Unit] =
    model.list match {
      case Some(list) if list.nonEmpty =>
        val pageSize = GalleryLayout.defaultPageSize
        val start    = (model.currentPage - 1) * pageSize
        val slice    = list.slice(start, start + pageSize)
        slice.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
      case _ => IO.unit
    }

  private def drawPreview(stored: StoredGridConfig): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"grid-preview-${stored.id}",
      framesToWait = 1,
      maxRetries = 100,
      delayMs = 3
    )((_: Canvas, ctx: CanvasRenderingContext2D) => drawGridScaled(ctx, stored.config))

  private def drawGridScaled(ctx: CanvasRenderingContext2D, grid: GridConfig): Unit = {
    ctx.fillStyle = "#eee"
    ctx.fillRect(0, 0, previewWidth, previewHeight)
    if (grid.parts.nonEmpty && grid.width > 0 && grid.height > 0) {
      val scale = (previewWidth.toDouble / grid.width).min(previewHeight.toDouble / grid.height)
      ctx.save()
      ctx.scale(scale, scale)
      ctx.lineWidth = (1.0 / scale).max(0.5)
      grid.parts.zipWithIndex.foreach { case (part, i) =>
        ctx.fillStyle = if (i % 2 == 0) "#e0e0e0" else "#c8c8c8"
        ctx.fillRect(part.x, part.y, part.width, part.height)
        ctx.strokeStyle = "#444"
        ctx.strokeRect(part.x, part.y, part.width, part.height)
      }
      ctx.restore()
    }
  }

}

final case class GridConfigGalleryModel(
    list: Option[List[StoredGridConfig]],
    pendingDeleteId: Option[String],
    currentPage: Int
)

enum GridConfigGalleryMsg:
  case Loaded(list: List[StoredGridConfig])
  case Edit(stored: StoredGridConfig)
  case Delete(stored: StoredGridConfig)
  case ConfirmDelete(id: String)
  case CancelDelete
  case DrawPreview(stored: StoredGridConfig)
  case CreateNew
  case PreviousPage
  case NextPage
  case Back
