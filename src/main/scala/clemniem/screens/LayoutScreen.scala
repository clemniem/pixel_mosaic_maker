package clemniem.screens

import cats.effect.IO
import clemniem.{ColumnDef, Layout, GridDefMode, NavigateNext, RowDef, Screen, ScreenId, ScreenOutput, StoredLayout, StorageKeys}
import clemniem.common.CanvasUtils
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Step 1: Define layout of sections. Define by rows (height + widths per row) or by columns (width + heights per column). */
object LayoutScreen extends Screen {
  type Model = LayoutModel
  type Msg   = LayoutMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LayoutId

  private val defaultRowHeight = 16
  private val defaultCellWidth = 32
  private val defaultColWidth  = 32
  private val defaultCellHeight = 16
  private def clampSize(n: Int): Int = math.max(1, math.min(500, n))

  /** Infer row definitions from a saved grid (for old saves that only have config, not rowDefs). */
  private def inferRowDefsFromConfig(config: Layout): List[RowDef] =
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
  private def inferColumnDefsFromConfig(config: Layout): List[ColumnDef] =
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
      case Some(ScreenOutput.EditLayout(stored)) =>
        val mode = stored.mode.getOrElse(GridDefMode.ByRows)
        val rowDefs =
          stored.rowDefs.filter(_.nonEmpty).getOrElse(inferRowDefsFromConfig(stored.config))
        val columnDefs =
          stored.columnDefs.filter(_.nonEmpty).getOrElse(inferColumnDefsFromConfig(stored.config))
        LayoutModel(
          mode = mode,
          rowDefs = rowDefs,
          columnDefs = columnDefs,
          name = stored.name,
          editingId = Some(stored.id)
        )
      case _ =>
        LayoutModel(
          mode = GridDefMode.ByRows,
          rowDefs = List(RowDef(defaultRowHeight, List(defaultCellWidth))),
          columnDefs = List(ColumnDef(defaultColWidth, List(defaultCellHeight))),
          name = "Unnamed layout",
          editingId = None
        )
    }
    (model, Cmd.SideEffect(drawGrid(model.grid)))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LayoutMsg.SetMode(mode) =>
      val next = model.copy(mode = mode)
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.AddRow =>
      val newRow = model.rowDefs.lastOption match {
        case Some(prev) => RowDef(prev.height, prev.cellWidths)
        case None       => RowDef(defaultRowHeight, List(defaultCellWidth))
      }
      val next = model.copy(rowDefs = model.rowDefs :+ newRow)
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.RemoveRow(idx) =>
      val next = model.copy(
        rowDefs = model.rowDefs.patch(idx, Nil, 1),
        anchoredRows = model.anchoredRows.filter(_ != idx).map(i => if (i > idx) i - 1 else i)
      )
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.SetRowHeight(rowIdx, raw) =>
      val h = raw.toIntOption.map(clampSize).getOrElse(defaultRowHeight)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, model.rowDefs(rowIdx).copy(height = h)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.SetRowCellWidth(rowIdx, cellIdx, raw) =>
      val w = raw.toIntOption.map(clampSize).getOrElse(defaultCellWidth)
      val row = model.rowDefs(rowIdx)
      val ws =
        if (model.anchoredRows.contains(rowIdx))
          List.fill(row.cellWidths.length)(w)
        else
          row.cellWidths.patch(cellIdx, List(w), 1)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = ws)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.AddCellToRow(rowIdx) =>
      val row = model.rowDefs(rowIdx)
      val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = row.cellWidths :+ defaultCellWidth)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.RemoveCellFromRow(rowIdx, cellIdx) =>
      val row = model.rowDefs(rowIdx)
      if (row.cellWidths.length <= 1) (model, Cmd.None)
      else {
        val ws = row.cellWidths.patch(cellIdx, Nil, 1)
        val next = model.copy(rowDefs = model.rowDefs.updated(rowIdx, row.copy(cellWidths = ws)))
        (next, Cmd.SideEffect(drawGrid(next.grid)))
      }

    case LayoutMsg.ToggleRowAnchor(rowIdx) =>
      val next = model.copy(
        anchoredRows = if (model.anchoredRows.contains(rowIdx)) model.anchoredRows - rowIdx else model.anchoredRows + rowIdx
      )
      (next, Cmd.None)

    case LayoutMsg.AddColumn =>
      val newCol = model.columnDefs.lastOption match {
        case Some(prev) => ColumnDef(prev.width, prev.cellHeights)
        case None       => ColumnDef(defaultColWidth, List(defaultCellHeight))
      }
      val next = model.copy(columnDefs = model.columnDefs :+ newCol)
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.RemoveColumn(idx) =>
      val next = model.copy(
        columnDefs = model.columnDefs.patch(idx, Nil, 1),
        anchoredColumns = model.anchoredColumns.filter(_ != idx).map(i => if (i > idx) i - 1 else i)
      )
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.SetColumnWidth(colIdx, raw) =>
      val w = raw.toIntOption.map(clampSize).getOrElse(defaultColWidth)
      val col = model.columnDefs(colIdx)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(width = w)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.SetColumnCellHeight(colIdx, cellIdx, raw) =>
      val h = raw.toIntOption.map(clampSize).getOrElse(defaultCellHeight)
      val col = model.columnDefs(colIdx)
      val hs =
        if (model.anchoredColumns.contains(colIdx))
          List.fill(col.cellHeights.length)(h)
        else
          col.cellHeights.patch(cellIdx, List(h), 1)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = hs)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.AddCellToColumn(colIdx) =>
      val col = model.columnDefs(colIdx)
      val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = col.cellHeights :+ defaultCellHeight)))
      (next, Cmd.SideEffect(drawGrid(next.grid)))

    case LayoutMsg.RemoveCellFromColumn(colIdx, cellIdx) =>
      val col = model.columnDefs(colIdx)
      if (col.cellHeights.length <= 1) (model, Cmd.None)
      else {
        val hs = col.cellHeights.patch(cellIdx, Nil, 1)
        val next = model.copy(columnDefs = model.columnDefs.updated(colIdx, col.copy(cellHeights = hs)))
        (next, Cmd.SideEffect(drawGrid(next.grid)))
      }

    case LayoutMsg.ToggleColumnAnchor(colIdx) =>
      val next = model.copy(
        anchoredColumns = if (model.anchoredColumns.contains(colIdx)) model.anchoredColumns - colIdx else model.anchoredColumns + colIdx
      )
      (next, Cmd.None)

    case LayoutMsg.DrawGrid =>
      (model, Cmd.SideEffect(drawGrid(model.grid)))

    case LayoutMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.LayoutsId, None)))

    case LayoutMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case LayoutMsg.Save =>
      if (model.pendingNormalizeChoice) (model, Cmd.None)
      else if (!model.isNormalized)
        (model.copy(pendingNormalizeChoice = true), Cmd.None)
      else {
        val cmd = LocalStorageUtils.loadList(StorageKeys.layouts)(
          LayoutMsg.LoadedForSave.apply,
          _ => LayoutMsg.LoadedForSave(Nil),
          (_, _) => LayoutMsg.LoadedForSave(Nil)
        )
        (model, cmd)
      }

    case LayoutMsg.NormalizeWithEnlarging =>
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

    case LayoutMsg.NormalizeWithNewSections =>
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

    case LayoutMsg.CancelNormalizeChoice =>
      (model.copy(pendingNormalizeChoice = false), Cmd.None)

    case LayoutMsg.LoadedForSave(list) =>
      val normalizedConfig = model.normalizedGrid
      val id                = model.editingId.getOrElse("grid-" + js.Date.now().toLong)
      val stored            = StoredLayout(
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
      val saveCmd = LocalStorageUtils.saveList(StorageKeys.layouts, newList)(
        _ => NavigateNext(ScreenId.LayoutsId, None),
        (msg, _) => LayoutMsg.SaveFailed(msg)
      )
      (model, saveCmd)

    case LayoutMsg.SaveFailed(_) =>
      (model, Cmd.None)

    case LayoutMsg.NoOp =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val grid = model.grid
    div(
      `class` := s"${NesCss.screenContainer} screen-container--wide"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row")(
          GalleryLayout.backButton(LayoutMsg.Back, "Layouts"),
          button(`class` := NesCss.btnPrimary, onClick(LayoutMsg.Save))(text("Save"))
        ),
        Some(ScreenHeader.nameRowInput(model.name, LayoutMsg.SetName.apply, None, "")),
        false
      ),
      div(
        `class` := s"normalize-choice-box ${if (model.pendingNormalizeChoice) "" else "hidden"}"
      )(
        p(`class` := NesCss.text, style := "margin: 0 0 10px 0; font-weight: 500;")(
          text("Your layout doesn't line up (rows or columns have different lengths). Choose how to fix it, then Save.")
        ),
        div(`class` := "flex-row")(
          button(`class` := NesCss.btnPrimary, onClick(LayoutMsg.NormalizeWithEnlarging))(text("Stretch existing sections")),
          button(`class` := NesCss.btnSuccess, onClick(LayoutMsg.NormalizeWithNewSections))(text("Add new sections to fill gaps")),
          button(`class` := NesCss.btn, onClick(LayoutMsg.CancelNormalizeChoice))(text("Cancel"))
        )
      ),
      p(`class` := s"${NesCss.text} field-block")(
        text("Set up how your mosaic is split into sections (like LEGO baseplates). You can define by rows or columns; each can have a different number of sections.")
      ),
      div(`class` := "field-block flex-row")(
        span()(text("Set up by:")),
        button(
          `class` := (if (model.mode == GridDefMode.ByRows) s"${NesCss.btn} is-primary" else NesCss.btn),
          onClick(LayoutMsg.SetMode(GridDefMode.ByRows))
        )(text("Rows")),
        button(
          `class` := (if (model.mode == GridDefMode.ByColumns) s"${NesCss.btn} is-primary" else NesCss.btn),
          onClick(LayoutMsg.SetMode(GridDefMode.ByColumns))
        )(text("Columns"))
      ),
      div(`class` := "field-block")(
        if (model.mode == GridDefMode.ByRows) rowsEditor(model.rowDefs, model.anchoredRows)
        else columnsEditor(model.columnDefs, model.anchoredColumns)
      ),
      div(`class` := s"${NesCss.containerRounded} grid-preview-box")(
        p(`class` := "section-title")(
          text(s"Preview · ${grid.width}×${grid.height} pixels · ${grid.parts.length} section(s)")
        ),
        div(onLoad(LayoutMsg.DrawGrid))(
          canvas(
            id := "grid-canvas",
            width := grid.width,
            height := grid.height,
            `class` := "pixel-canvas"
          )()
        )
      )
    )
  }

  private def rowsEditor(rowDefs: List[RowDef], anchoredRows: Set[Int]): Html[Msg] = {
    val rowElems = rowDefs.zipWithIndex.toList.map { case (row, rowIdx) =>
      val heightInput = input(
        `type` := "number",
        min := "1",
        max := "500",
        value := row.height.toString,
        onInput(s => LayoutMsg.SetRowHeight(rowIdx, s)),
        `class` := s"${NesCss.input} input-w-4"
      )
      val cellInputs = row.cellWidths.zipWithIndex.map { case (w, cellIdx) =>
        input(
          `type` := "number",
          min := "1",
          max := "500",
          value := w.toString,
          onInput(s => LayoutMsg.SetRowCellWidth(rowIdx, cellIdx, s)),
          `class` := s"${NesCss.input} input-w-3half"
        )
      }
      div(`class` := s"${NesCss.containerRounded} grid-editor-row")(
        div(`class` := "grid-editor-row-first")(
          button(`class` := NesCss.btn, onClick(LayoutMsg.RemoveRow(rowIdx)))(text("− row")),
          heightInput,
          button(
            `class` := (if (row.cellWidths.length <= 1) s"${NesCss.btn} btn-disabled" else NesCss.btn),
            onClick(LayoutMsg.RemoveCellFromRow(rowIdx, row.cellWidths.length - 1)),
            title := "Remove last section"
          )(text("−")),
          button(
            `class` := (if (anchoredRows.contains(rowIdx)) s"${NesCss.btn} is-primary" else NesCss.btn),
            onClick(LayoutMsg.ToggleRowAnchor(rowIdx)),
            title := "When on, all sections in this row share the same width"
          )(text("≡")),
          button(`class` := NesCss.btn, onClick(LayoutMsg.AddCellToRow(rowIdx)), title := "Add section")(text("+"))
        ),
        div(`class` := "grid-editor-row-second")(
          div(`class` := "grid-editor-cells")(cellInputs*)
        )
      )
    }
    val addRowBtn = button(`class` := NesCss.btnPrimary, onClick(LayoutMsg.AddRow))(text("+ Add row"))
    div(`class` := "grid-editor-list")((rowElems :+ addRowBtn)*)
  }

  private def columnsEditor(colDefs: List[ColumnDef], anchoredColumns: Set[Int]): Html[Msg] = {
    val colElems = colDefs.zipWithIndex.toList.map { case (col, colIdx) =>
      val widthInput = input(
        `type` := "number",
        min := "1",
        max := "500",
        value := col.width.toString,
        onInput(s => LayoutMsg.SetColumnWidth(colIdx, s)),
        `class` := s"${NesCss.input} input-w-4"
      )
      val cellInputs = col.cellHeights.zipWithIndex.map { case (h, cellIdx) =>
        input(
          `type` := "number",
          min := "1",
          max := "500",
          value := h.toString,
          onInput(s => LayoutMsg.SetColumnCellHeight(colIdx, cellIdx, s)),
          `class` := s"${NesCss.input} input-w-3half"
        )
      }
      div(`class` := s"${NesCss.containerRounded} grid-editor-row")(
        div(`class` := "grid-editor-row-first")(
          button(`class` := NesCss.btn, onClick(LayoutMsg.RemoveColumn(colIdx)))(text("− col")),
          widthInput,
          button(
            `class` := (if (col.cellHeights.length <= 1) s"${NesCss.btn} btn-disabled" else NesCss.btn),
            onClick(LayoutMsg.RemoveCellFromColumn(colIdx, col.cellHeights.length - 1)),
            title := "Remove last section"
          )(text("−")),
          button(
            `class` := (if (anchoredColumns.contains(colIdx)) s"${NesCss.btn} is-primary" else NesCss.btn),
            onClick(LayoutMsg.ToggleColumnAnchor(colIdx)),
            title := "When on, all sections in this column share the same height"
          )(text("≡")),
          button(`class` := NesCss.btn, onClick(LayoutMsg.AddCellToColumn(colIdx)), title := "Add section")(text("+"))
        ),
        div(`class` := "grid-editor-row-second")(
          div(`class` := "grid-editor-cells")(cellInputs*)
        )
      )
    }
    val addColBtn = button(`class` := NesCss.btnPrimary, onClick(LayoutMsg.AddColumn))(text("+ Add column"))
    div(`class` := "grid-editor-list")((colElems :+ addColBtn)*)
  }

  /** Draw grid on canvas after the view has been applied (next frame + retries). Use for all updates. */
  def drawGrid(grid: Layout): IO[Unit] =
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

final case class LayoutModel(
    mode: GridDefMode,
    rowDefs: List[RowDef],
    columnDefs: List[ColumnDef],
    name: String,
    editingId: Option[String] = None,
    pendingNormalizeChoice: Boolean = false,
    anchoredRows: Set[Int] = Set.empty,
    anchoredColumns: Set[Int] = Set.empty
) {
  def grid: Layout =
    mode match
      case GridDefMode.ByRows    => Layout.fromRowDefs(rowDefs)
      case GridDefMode.ByColumns => Layout.fromColumnDefs(columnDefs)

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
  def normalizedGrid: Layout =
    mode match
      case GridDefMode.ByRows    => Layout.fromRowDefs(RowDef.normalizeToRectangle(rowDefs))
      case GridDefMode.ByColumns => Layout.fromColumnDefs(ColumnDef.normalizeToRectangle(columnDefs))
}

enum LayoutMsg:
  case SetMode(mode: GridDefMode)
  case AddRow
  case RemoveRow(idx: Int)
  case SetRowHeight(rowIdx: Int, value: String)
  case SetRowCellWidth(rowIdx: Int, cellIdx: Int, value: String)
  case AddCellToRow(rowIdx: Int)
  case RemoveCellFromRow(rowIdx: Int, cellIdx: Int)
  case ToggleRowAnchor(rowIdx: Int)
  case AddColumn
  case RemoveColumn(idx: Int)
  case SetColumnWidth(colIdx: Int, value: String)
  case SetColumnCellHeight(colIdx: Int, cellIdx: Int, value: String)
  case AddCellToColumn(colIdx: Int)
  case RemoveCellFromColumn(colIdx: Int, cellIdx: Int)
  case ToggleColumnAnchor(colIdx: Int)
  case DrawGrid
  case SetName(name: String)
  case Save
  case LoadedForSave(list: List[StoredLayout])
  case SaveFailed(message: String)
  case NormalizeWithEnlarging
  case NormalizeWithNewSections
  case CancelNormalizeChoice
  case Back
  case NoOp
