package clemniem.screens

import cats.effect.IO
import clemniem.{GridConfig, NavigateNext, Screen, ScreenId, ScreenOutput, StoredGridConfig, StorageKeys}
import clemniem.common.CanvasUtils
import clemniem.common.LocalStorageUtils
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

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    model.list match {
      case None =>
        div(style := container)(p(text("Loading…")))
      case Some(list) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Grid configs")),
            button(
              style := "padding: 6px 12px; cursor: pointer;",
              onClick(GridConfigGalleryMsg.Back)
            )(text("← Overview"))
          ),
          if (list.isEmpty)
            emptyState("Create GridConfig", GridConfigGalleryMsg.CreateNew)
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map { item =>
                entryCard(item, model.pendingDeleteId.contains(item.id))
              } :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(GridConfigGalleryMsg.CreateNew)
              )(text("+ Create GridConfig")))*
            )
        )
    }
  }

  private def entryCard(item: StoredGridConfig, confirmingDelete: Boolean): Html[Msg] =
    div(
      style := "display: flex; align-items: center; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(onLoad(GridConfigGalleryMsg.DrawPreview(item)))(
        canvas(
          id := s"grid-preview-${item.id}",
          width := previewWidth,
          height := previewHeight,
          style := "border: 1px solid #999; border-radius: 2px; flex-shrink: 0;"
        )()
      ),
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(s"${item.config.width}×${item.config.height} · ${item.config.parts.length} plate(s)")
        ),
        if (confirmingDelete)
          div(style := "margin-top: 0.5rem; padding: 6px 0;")(
            span(style := "font-size: 0.875rem; color: #b71c1c; margin-right: 8px;")(text(s"Delete \"${item.name}\"?")),
            button(
              style := "padding: 4px 10px; margin-right: 6px; cursor: pointer; background: #b71c1c; color: #fff; border: none; border-radius: 4px; font-size: 0.875rem;",
              onClick(GridConfigGalleryMsg.ConfirmDelete(item.id))
            )(text("Yes")),
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #999; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(GridConfigGalleryMsg.CancelDelete)
            )(text("Cancel"))
          )
        else
          div(style := "margin-top: 0.5rem; display: flex; gap: 6px;")(
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #555; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(GridConfigGalleryMsg.Edit(item))
            )(text("Edit")),
            button(
              style := "padding: 4px 10px; cursor: pointer; border: 1px solid #b71c1c; color: #b71c1c; border-radius: 4px; font-size: 0.875rem; background: #fff;",
              onClick(GridConfigGalleryMsg.Delete(item))
            )(text("Delete"))
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

  private def emptyState(createLabel: String, createMsg: Msg): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text("No grid configs yet.")),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(s"+ $createLabel"))
    )
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
