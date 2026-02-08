package clemniem.screens

import cats.effect.IO
import clemniem.{ColumnDef, GridConfig, GridDefMode, NavigateNext, RowDef, Screen, ScreenId, ScreenOutput, StoredGridConfig, StorageKeys}
import clemniem.common.CanvasUtils
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Step 1: Define grid of plates (Lego-style). Define by rows (height + widths per row) or by columns (width + heights per column). */
object GridConfigScreen extends Screen {
  type Model = GridConfigModel
  type Msg   = GridConfigMsg | NavigateNext

  val screenId: ScreenId = ScreenId.GridConfigId

  private val defaultRowHeight = 16
  private val defaultCellWidth = 48
  private val defaultColWidth  = 48
  private val defaultCellHeight = 16
  private def clampSize(n: Int): Int = math.max(1, math.min(500, n))

  /** Infer row definitions from a saved grid (for old saves that only have config, not rowDefs). */
  private def inferRowDefsFromConfig(config: GridConfig): List[RowDef] =
    if (config.parts.isEmpty) List(RowDef(defaultRowHeight, List(defaultCellWidth)))
    else {
      val byRow = config.parts.groupBy(_.y).toList.sortBy(_._1).map(_._2.toList.sortBy(_.x))
      byRow.map { rowParts =>
        val h      = rowParts.head.height
        val widths = rowParts.map(_.width)
        RowDef(h, widths)
      }
    }

  /** Infer column definitions from a saved grid (for old saves that only have config, not columnDefs). */
  private def inferColumnDefsFromConfig(config: GridConfig): List[ColumnDef] =
    if (config.parts.isEmpty) List(ColumnDef(defaultColWidth, List(defaultCellHeight)))
    else {
      val byCol = config.parts.groupBy(_.x).toList.sortBy(_._1).map(_._2.toList.sortBy(_.y))
      byCol.map { colParts =>
        val w       = colParts.head.width
        val heights = colParts.map(_.height)
        ColumnDef(w, heights)
      }
    }

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = previous match {
      case Some(ScreenOutput.EditGridConfig(stored)) =>
        val mode = stored.mode.getOrElse(GridDefMode.ByRows)
        val rowDefs =
          stored.rowDefs.filter(_.nonEmpty).getOrElse(inferRowDefsFromConfig(stored.config))
        val columnDefs =
          stored.columnDefs.filter(_.nonEmpty).getOrElse(inferColumnDefsFromConfig(stored.config))
        GridConfigModel(
          mode = mode,
          rowDefs = rowDefs,
          columnDefs = columnDefs,
          name = stored.name,
          editingId = Some(stored.id)
        )
      case _ =>
        GridConfigModel(
          mode = GridDefMode.ByRows,
          rowDefs = List(RowDef(defaultRowHeight, List(defaultCellWidth))),
          columnDefs = List(ColumnDef(defaultColWidth, List(defaultCellHeight))),
          name = "Unnamed grid",
          editingId = None
        )
    }
    (model, Cmd.SideEffect(drawGrid(model.grid)))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case GridConfigMsg.SetMode(mode) =>
      val next = model.copy(mode = mode)
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.AddRow =>
      val next = model.copy(rowDefs = model.rowDefs :+ RowDef(defaultRowHeight, List(defaultCellWidth)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.RemoveRow(idx) =>
      val next = model.copy(rowDefs = model.rowDefs.patch(idx, Nil, 1))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.SetRowHeight(rowIdx, raw) =>
      val h = raw.toIntOption.map(clampSize).getOrElse(defaultRowHeight)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, model.rowDefs(rowIdx).copy(height = h)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.SetRowCellWidth(rowIdx, cellIdx, raw) =>
      val w = raw.toIntOption.map(clampSize).getOrElse(defaultCellWidth)
      val row = model.rowDefs(rowIdx)
      val ws = row.cellWidths.patch(cellIdx, List(w), 1)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = ws)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.AddCellToRow(rowIdx) =>
      val row = model.rowDefs(rowIdx)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = row.cellWidths :+ defaultCellWidth)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.RemoveCellFromRow(rowIdx, cellIdx) =>
      val row = model.rowDefs(rowIdx)
      if (row.cellWidths.length <= 1) (model, Cmd.None)
      else {
        val ws = row.cellWidths.patch(cellIdx, Nil, 1)
        val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = ws)))
        (next, Cmd.SideEffect(drawGrid(next.grid)))
      }

    case GridConfigMsg.AddColumn =>
      val next = model.copy(columnDefs = model.columnDefs :+ ColumnDef(defaultColWidth, List(defaultCellHeight)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.RemoveColumn(idx) =>
      val next = model.copy(columnDefs = model.columnDefs.patch(idx, Nil, 1))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.SetColumnWidth(colIdx, raw) =>
      val w = raw.toIntOption.map(clampSize).getOrElse(defaultColWidth)
      val col = model.columnDefs(colIdx)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(width = w)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.SetColumnCellHeight(colIdx, cellIdx, raw) =>
      val h = raw.toIntOption.map(clampSize).getOrElse(defaultCellHeight)
      val col = model.columnDefs(colIdx)
      val hs = col.cellHeights.patch(cellIdx, List(h), 1)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = hs)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.AddCellToColumn(colIdx) =>
      val col = model.columnDefs(colIdx)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = col.cellHeights :+ defaultCellHeight)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.RemoveCellFromColumn(colIdx, cellIdx) =>
      val col = model.columnDefs(colIdx)
      if (col.cellHeights.length <= 1) (model, Cmd.None)
      else {
        val hs = col.cellHeights.patch(cellIdx, Nil, 1)
        val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = hs)))
        (next, Cmd.SideEffect(drawGrid(next.grid)))
      }

    case GridConfigMsg.DrawGrid =>
      (model, Cmd.SideEffect(drawGrid(model.grid)))

    case GridConfigMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GridConfigsId, None)))

    case GridConfigMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case GridConfigMsg.Save =>
      if (model.pendingNormalizeChoice) (model, Cmd.None)
      else if (!model.isNormalized)
        (model.copy(pendingNormalizeChoice = true), Cmd.None)
      else {
        val cmd = LocalStorageUtils.loadList(StorageKeys.gridConfigs)(
          GridConfigMsg.LoadedForSave.apply,
          _ => GridConfigMsg.LoadedForSave(Nil),
          (_, _) => GridConfigMsg.LoadedForSave(Nil)
        )
        (model, cmd)
      }

    case GridConfigMsg.NormalizeWithEnlarging =>
      val next = model.copy(
        rowDefs =
          if (model.mode == GridDefMode.ByRows) RowDef.normalizeByEnlarging(model.rowDefs)
          else model.rowDefs,
        columnDefs =
          if (model.mode == GridDefMode.ByColumns) ColumnDef.normalizeByEnlarging(model.columnDefs)
          else model.columnDefs,
        pendingNormalizeChoice = false
      )
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.NormalizeWithNewPlates =>
      val next = model.copy(
        rowDefs =
          if (model.mode == GridDefMode.ByRows) RowDef.normalizeToRectangle(model.rowDefs)
          else model.rowDefs,
        columnDefs =
          if (model.mode == GridDefMode.ByColumns) ColumnDef.normalizeToRectangle(model.columnDefs)
          else model.columnDefs,
        pendingNormalizeChoice = false
      )
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case GridConfigMsg.CancelNormalizeChoice =>
      (model.copy(pendingNormalizeChoice = false), Cmd.None)

    case GridConfigMsg.LoadedForSave(list) =>
      val normalizedConfig = model.normalizedGrid
      val id                = model.editingId.getOrElse("grid-" + js.Date.now().toLong)
      val stored            = StoredGridConfig(
        id = id,
        name = model.name,
        config = normalizedConfig,
        mode = Some(model.mode),
        rowDefs = Some(model.rowDefs),
        columnDefs = Some(model.columnDefs)
      )
      val newList = model.editingId match {
        case Some(editId) => list.filterNot(_.id == editId) :+ stored
        case None         => list :+ stored
      }
      val saveCmd = LocalStorageUtils.saveList(StorageKeys.gridConfigs, newList)(
        _ => NavigateNext(ScreenId.GridConfigsId, None),
        (msg, _) => GridConfigMsg.SaveFailed(msg)
      )
      (model, saveCmd)

    case GridConfigMsg.SaveFailed(_) =>
      (model, Cmd.None)

    case GridConfigMsg.NoOp =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val grid = model.grid
    div(
      style := "font-family: system-ui, sans-serif; max-width: 56rem; margin: 0 auto; padding: 1rem;"
    )(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
        h2(style := "margin: 0;")(text(screenId.title)),
        div(style := "display: flex; align-items: center; gap: 8px;")(
          button(style := "padding: 6px 12px; cursor: pointer;", onClick(GridConfigMsg.Back))(
            text("← GridConfigs")
          ),
          input(
            `type` := "text",
            placeholder := "Name",
            value := model.name,
            onInput(GridConfigMsg.SetName.apply),
            style := "padding: 6px 10px; width: 12rem; border: 1px solid #ccc; border-radius: 4px;"
          ),
          button(
            style := "padding: 6px 14px; cursor: pointer; background: #2e7d32; color: #fff; border: none; border-radius: 4px; font-weight: 500;",
            onClick(GridConfigMsg.Save)
          )(text("Save"))
        )
      ),
      div(
        style := (if (model.pendingNormalizeChoice) "margin-bottom: 1rem; padding: 12px; background: #fff3e0; border: 1px solid #ffb74d; border-radius: 6px;"
         else "display: none;")
      )(
        p(style := "margin: 0 0 10px 0; font-weight: 500;")(
          text("Grid is not a rectangle (rows have different total widths or columns different heights). Choose how to fix it; then press Save.")
        ),
        div(style := "display: flex; flex-wrap: wrap; gap: 8px; align-items: center;")(
            button(
              style := "padding: 6px 14px; cursor: pointer; background: #1565c0; color: #fff; border: none; border-radius: 4px;",
              onClick(GridConfigMsg.NormalizeWithEnlarging)
            )(text("Enlarge existing cells")),
            button(
              style := "padding: 6px 14px; cursor: pointer; background: #2e7d32; color: #fff; border: none; border-radius: 4px;",
              onClick(GridConfigMsg.NormalizeWithNewPlates)
            )(text("Add new plates to fill gaps")),
          button(
            style := "padding: 6px 14px; cursor: pointer; background: #fff; border: 1px solid #ccc; border-radius: 4px;",
            onClick(GridConfigMsg.CancelNormalizeChoice)
          )(text("Cancel"))
        )
      ),
      p(style := "color: #444; margin-bottom: 1rem;")(
        text("Define the grid of plates (Lego-style). Instructions are generated per plate. Choose by rows or by columns; each row/column can have a different number of cells.")
      ),
      div(style := "margin-bottom: 1rem;")(
        span(style := "margin-right: 0.5rem;")(text("Define by:")),
        button(
          style := styleForMode(model.mode == GridDefMode.ByRows),
          onClick(GridConfigMsg.SetMode(GridDefMode.ByRows))
        )(text("Rows")),
        button(
          style := styleForMode(model.mode == GridDefMode.ByColumns),
          onClick(GridConfigMsg.SetMode(GridDefMode.ByColumns))
        )(text("Columns"))
      ),
      div(style := "margin-bottom: 1rem;")(
        if (model.mode == GridDefMode.ByRows) rowsEditor(model.rowDefs)
        else columnsEditor(model.columnDefs)
      ),
      div(style := "margin-top: 1rem; border: 1px solid #ccc; border-radius: 4px; padding: 8px; background: #fafafa;")(
        p(style := "margin: 0 0 8px 0; font-size: 0.9rem;")(
          text(s"Preview · ${grid.width}×${grid.height} px · ${grid.parts.length} plate(s)")
        ),
        div(onLoad(GridConfigMsg.DrawGrid))(
          canvas(
            id := "grid-canvas",
            width := grid.width,
            height := grid.height,
            style := "border: 1px solid #333; display: block; max-width: 100%;"
          )()
        )
      )
      // Canvas is a stable node (same id); Tyrian patches width/height. All drawing is done via Cmd
      // in drawGrid() using drawAfterViewReady (next frame + retries), not by replacing the node.
    )
  }

  private def styleForMode(active: Boolean): String =
    if (active) "padding: 6px 12px; font-weight: bold; background: #333; color: #fff; border: 1px solid #333; border-radius: 4px; cursor: pointer;"
    else "padding: 6px 12px; background: #fff; border: 1px solid #ccc; border-radius: 4px; cursor: pointer;"

  private def rowsEditor(rowDefs: List[RowDef]): Html[Msg] = {
    val rowElems = rowDefs.zipWithIndex.toList.map { case (row, rowIdx) =>
      val heightInput = input(
        `type` := "number",
        min := "1",
        max := "500",
        value := row.height.toString,
        onInput(s => GridConfigMsg.SetRowHeight(rowIdx, s)),
        style := "width: 4rem; padding: 4px; margin-right: 8px;"
      )
      val cellInputs = row.cellWidths.zipWithIndex.map { case (w, cellIdx) =>
        input(
          `type` := "number",
          min := "1",
          max := "500",
          value := w.toString,
          onInput(s => GridConfigMsg.SetRowCellWidth(rowIdx, cellIdx, s)),
          style := "width: 3.5rem; padding: 4px; margin-right: 4px;"
        )
      }
      div(
        style := "display: flex; align-items: center; flex-wrap: wrap; gap: 8px; padding: 8px; background: #f5f5f5; border-radius: 4px;"
      )(
        span(style := "font-size: 0.85rem; min-width: 3rem;")(text(s"Row ${rowIdx + 1}")),
        heightInput,
        span(style := "font-size: 0.8rem; color: #666;")(text("px height")),
        span(style := "font-size: 0.8rem; color: #888; margin-left: 4px;")(text("widths:")),
        div(style := "display: flex; align-items: center; flex-wrap: wrap; gap: 4px;")(cellInputs*),
        button(
          onClick(GridConfigMsg.AddCellToRow(rowIdx)),
          style := "padding: 4px 8px;"
        )(text("+ cell")),
        button(
          onClick(GridConfigMsg.RemoveRow(rowIdx)),
          style := "padding: 4px 8px;"
        )(text("− row"))
      )
    }
    val addRowBtn = button(
      onClick(GridConfigMsg.AddRow),
      style := "padding: 8px 16px; align-self: flex-start;"
    )(text("+ Add row"))
    div(
      style := "display: flex; flex-direction: column; gap: 12px;"
    )((rowElems :+ addRowBtn)*)
  }

  private def columnsEditor(colDefs: List[ColumnDef]): Html[Msg] = {
    val colElems = colDefs.zipWithIndex.toList.map { case (col, colIdx) =>
      val widthInput = input(
        `type` := "number",
        min := "1",
        max := "500",
        value := col.width.toString,
        onInput(s => GridConfigMsg.SetColumnWidth(colIdx, s)),
        style := "width: 4rem; padding: 4px; margin-right: 8px;"
      )
      val cellInputs = col.cellHeights.zipWithIndex.map { case (h, cellIdx) =>
        input(
          `type` := "number",
          min := "1",
          max := "500",
          value := h.toString,
          onInput(s => GridConfigMsg.SetColumnCellHeight(colIdx, cellIdx, s)),
          style := "width: 3.5rem; padding: 4px; margin-right: 4px;"
        )
      }
      div(
        style := "display: flex; align-items: center; flex-wrap: wrap; gap: 8px; padding: 8px; background: #f5f5f5; border-radius: 4px;"
      )(
        span(style := "font-size: 0.85rem; min-width: 3rem;")(text(s"Col ${colIdx + 1}")),
        widthInput,
        span(style := "font-size: 0.8rem; color: #666;")(text("px width")),
        span(style := "font-size: 0.8rem; color: #888; margin-left: 4px;")(text("heights:")),
        div(style := "display: flex; align-items: center; flex-wrap: wrap; gap: 4px;")(cellInputs*),
        button(
          onClick(GridConfigMsg.AddCellToColumn(colIdx)),
          style := "padding: 4px 8px;"
        )(text("+ cell")),
        button(
          onClick(GridConfigMsg.RemoveColumn(colIdx)),
          style := "padding: 4px 8px;"
        )(text("− col"))
      )
    }
    val addColBtn = button(
      onClick(GridConfigMsg.AddColumn),
      style := "padding: 8px 16px; align-self: flex-start;"
    )(text("+ Add column"))
    div(
      style := "display: flex; flex-direction: column; gap: 12px;"
    )((colElems :+ addColBtn)*)
  }

  /** Draw grid on canvas after the view has been applied (next frame + retries). Use for all updates. */
  def drawGrid(grid: GridConfig): IO[Unit] =
    CanvasUtils.drawAfterViewReady("grid-canvas", maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      canvas.width = grid.width
      canvas.height = grid.height
      ctx.clearRect(0, 0, grid.width, grid.height)
      ctx.lineWidth = 1
      grid.parts.zipWithIndex.foreach { case (part, i) =>
        ctx.fillStyle = if (i % 2 == 0) "#f5f5f5" else "#eee"
        ctx.fillRect(part.x, part.y, part.width, part.height)
        ctx.strokeStyle = "#333"
        ctx.strokeRect(part.x, part.y, part.width, part.height)
      }
    })
}

final case class GridConfigModel(
    mode: GridDefMode,
    rowDefs: List[RowDef],
    columnDefs: List[ColumnDef],
    name: String,
    editingId: Option[String] = None,
    pendingNormalizeChoice: Boolean = false
) {
  def grid: GridConfig =
    mode match
      case GridDefMode.ByRows    => GridConfig.fromRowDefs(rowDefs)
      case GridDefMode.ByColumns => GridConfig.fromColumnDefs(columnDefs)

  /** True if grid is a full rectangle (all rows same total width, or all columns same total height). */
  def isNormalized: Boolean =
    mode match
      case GridDefMode.ByRows =>
        rowDefs.nonEmpty && rowDefs.forall(_.cellWidths.nonEmpty) &&
          rowDefs.map(_.totalWidth).distinct.size == 1
      case GridDefMode.ByColumns =>
        columnDefs.nonEmpty && columnDefs.forall(_.cellHeights.nonEmpty) &&
          columnDefs.map(_.totalHeight).distinct.size == 1

  /** Grid built from row/column defs normalized to a rectangle (no gaps at end of any row or column). */
  def normalizedGrid: GridConfig =
    mode match
      case GridDefMode.ByRows    => GridConfig.fromRowDefs(RowDef.normalizeToRectangle(rowDefs))
      case GridDefMode.ByColumns => GridConfig.fromColumnDefs(ColumnDef.normalizeToRectangle(columnDefs))
}

enum GridConfigMsg:
  case SetMode(mode: GridDefMode)
  case AddRow
  case RemoveRow(idx: Int)
  case SetRowHeight(rowIdx: Int, value: String)
  case SetRowCellWidth(rowIdx: Int, cellIdx: Int, value: String)
  case AddCellToRow(rowIdx: Int)
  case RemoveCellFromRow(rowIdx: Int, cellIdx: Int)
  case AddColumn
  case RemoveColumn(idx: Int)
  case SetColumnWidth(colIdx: Int, value: String)
  case SetColumnCellHeight(colIdx: Int, cellIdx: Int, value: String)
  case AddCellToColumn(colIdx: Int)
  case RemoveCellFromColumn(colIdx: Int, cellIdx: Int)
  case DrawGrid
  case SetName(name: String)
  case Save
  case LoadedForSave(list: List[StoredGridConfig])
  case SaveFailed(message: String)
  case NormalizeWithEnlarging
  case NormalizeWithNewPlates
  case CancelNormalizeChoice
  case Back
  case NoOp
