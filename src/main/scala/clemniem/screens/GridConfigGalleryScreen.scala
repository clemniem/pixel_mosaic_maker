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
    (GridConfigGalleryModel(None, None), cmd)
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
      (model.copy(list = Some(list)), drawPreviews)
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
          (model.copy(list = Some(newList), pendingDeleteId = None), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case GridConfigGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case GridConfigGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GridConfigId, None)))
    case GridConfigGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case GridConfigGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
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
            h1(`class` := "screen-title")(text("Grid configs")),
            button(`class` := NesCss.btn, onClick(GridConfigGalleryMsg.Back))(text("← Overview"))
          ),
          if (list.isEmpty)
            GalleryEmptyState("No grid configs yet.", "+ Create GridConfig", GridConfigGalleryMsg.CreateNew)
          else
            div(`class` := "flex-col flex-col--gap-tight")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+
                button(`class` := NesCss.btnPrimary, style := "margin-top: 0.5rem;", onClick(GridConfigGalleryMsg.CreateNew))(text("+ Create GridConfig")))*
            )
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
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text nes-text")(text(s"Delete \"${item.name}\"?")),
            button(`class` := NesCss.btnError, style := "margin-right: 6px;", onClick(GridConfigGalleryMsg.ConfirmDelete(item.id)))(text("Yes")),
            button(`class` := NesCss.btn, onClick(GridConfigGalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := NesCss.btn, onClick(GridConfigGalleryMsg.Edit(item)))(text("Edit")),
            button(`class` := NesCss.btnError, onClick(GridConfigGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )

  private val previewWidth  = 120
  private val previewHeight = 80

  private def drawPreview(stored: StoredGridConfig): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"grid-preview-${stored.id}",
      framesToWait = 2,
      maxRetries = 100,
      delayMs = 3
    )((canvas: Canvas, ctx: CanvasRenderingContext2D) => drawGridScaled(canvas, ctx, stored.config))

  private def drawGridScaled(canvas: Canvas, ctx: CanvasRenderingContext2D, grid: GridConfig): Unit = {
    canvas.width = previewWidth
    canvas.height = previewHeight
    ctx.clearRect(0, 0, previewWidth, previewHeight)
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
    pendingDeleteId: Option[String]
)

enum GridConfigGalleryMsg:
  case Loaded(list: List[StoredGridConfig])
  case Edit(stored: StoredGridConfig)
  case Delete(stored: StoredGridConfig)
  case ConfirmDelete(id: String)
  case CancelDelete
  case DrawPreview(stored: StoredGridConfig)
  case CreateNew
  case Back
