package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class GridPart(x: Int, y: Int, width: Int, height: Int)

object GridPart {
  given Encoder[GridPart] = deriveEncoder
  given Decoder[GridPart] = deriveDecoder
}

/** Definition mode: define grid by rows (fixed height per row + widths per cell) or by columns (fixed width per column + heights per cell). */
enum GridDefMode:
  case ByRows
  case ByColumns

object GridDefMode {
  given Encoder[GridDefMode] = deriveEncoder
  given Decoder[GridDefMode] = deriveDecoder
}

/** Shared normalization logic for row/column definitions that pad or enlarge cells to form a rectangle. */
private object GridDefNormalize {

  /** Pad each def so all have the same total dimension: add a new cell to fill the gap. */
  def normalizeToRect[A](defs: List[A], total: A => Int, cells: A => List[Int], update: (A, List[Int]) => A): List[A] =
    if (defs.isEmpty) defs
    else {
      val maxDim = defs.map(total).max.max(1)
      defs.map { d =>
        val gap = maxDim - total(d)
        if (gap <= 0) d
        else update(d, cells(d) :+ gap)
      }
    }

  /** Normalize by enlarging the last cell of each short def (no new plates). */
  def normalizeByEnlarge[A](defs: List[A], total: A => Int, cells: A => List[Int], update: (A, List[Int]) => A): List[A] =
    if (defs.isEmpty) defs
    else {
      val maxDim = defs.map(total).max.max(1)
      defs.map { d =>
        val gap = maxDim - total(d)
        if (gap <= 0 || cells(d).isEmpty) d
        else update(d, cells(d).dropRight(1) :+ (cells(d).last + gap))
      }
    }
}

/** One row: height of the row and width of each cell in the row (variable number of cells allowed). */
final case class RowDef(height: Int, cellWidths: List[Int]) {
  def totalWidth: Int = cellWidths.sum
}

object RowDef {
  given Encoder[RowDef] = deriveEncoder
  given Decoder[RowDef] = deriveDecoder

  /** Pad each row so all rows have the same total width (no gaps): add a new cell to fill the gap. */
  def normalizeToRectangle(rowDefs: List[RowDef]): List[RowDef] =
    GridDefNormalize.normalizeToRect(rowDefs, _.totalWidth, _.cellWidths, (r, cw) => r.copy(cellWidths = cw))

  /** Normalize by enlarging the last cell of each short row (no new plates). */
  def normalizeByEnlarging(rowDefs: List[RowDef]): List[RowDef] =
    GridDefNormalize.normalizeByEnlarge(rowDefs, _.totalWidth, _.cellWidths, (r, cw) => r.copy(cellWidths = cw))
}

/** One column: width of the column and height of each cell in the column (variable number of cells allowed). */
final case class ColumnDef(width: Int, cellHeights: List[Int]) {
  def totalHeight: Int = cellHeights.sum
}

object ColumnDef {
  given Encoder[ColumnDef] = deriveEncoder
  given Decoder[ColumnDef] = deriveDecoder

  /** Pad each column so all columns have the same total height (no gaps): add a new cell to fill the gap. */
  def normalizeToRectangle(colDefs: List[ColumnDef]): List[ColumnDef] =
    GridDefNormalize.normalizeToRect(colDefs, _.totalHeight, _.cellHeights, (c, ch) => c.copy(cellHeights = ch))

  /** Normalize by enlarging the last cell of each short column (no new plates). */
  def normalizeByEnlarging(colDefs: List[ColumnDef]): List[ColumnDef] =
    GridDefNormalize.normalizeByEnlarge(colDefs, _.totalHeight, _.cellHeights, (c, ch) => c.copy(cellHeights = ch))
}

final case class GridConfig(cols: Int, rows: Int, parts: Array[GridPart]) {

  /** Total width = bounding box (works for uniform and variable layouts). */
  val width: Int =
    if (parts.isEmpty) 0
    else parts.iterator.map(p => p.x + p.width).max

  /** Total height = bounding box (works for uniform and variable layouts). */
  val height: Int =
    if (parts.isEmpty) 0
    else parts.iterator.map(p => p.y + p.height).max
}

object GridConfig {
  given Encoder[GridConfig] = deriveEncoder
  given Decoder[GridConfig] = deriveDecoder

  /** Uniform grid: same column widths for all rows, same row heights for all columns. */
  def make(rowHeights: Seq[Int], columnWidths: Seq[Int]): GridConfig = {
    val rowOffsets  = rowHeights.scanLeft(0)(_ + _).dropRight(1)
    val colOffsets  = columnWidths.scanLeft(0)(_ + _).dropRight(1)
    val parts: Array[GridPart] =
      (for {
        (rowHeight, y) <- rowHeights.zip(rowOffsets)
        (colWidth, x)  <- columnWidths.zip(colOffsets)
      } yield GridPart(x = x, y = y, width = colWidth, height = rowHeight)).toArray
    GridConfig(cols = columnWidths.length, rows = rowHeights.length, parts = parts)
  }

  /** Build from row definitions: each row has a height and a list of cell widths (variable count per row). */
  def fromRowDefs(rowDefs: List[RowDef]): GridConfig = {
    if (rowDefs.isEmpty) GridConfig(0, 0, Array.empty)
    else {
      val cols = rowDefs.map(_.cellWidths.length).max
      val rows = rowDefs.length
      val (_, parts) = rowDefs.foldLeft((0, Vector.empty[GridPart])) { case ((y, acc), row) =>
        val (_, rowParts) = row.cellWidths.foldLeft((0, Vector.empty[GridPart])) { case ((x, pacc), w) =>
          (x + w, pacc :+ GridPart(x = x, y = y, width = w, height = row.height))
        }
        (y + row.height, acc ++ rowParts)
      }
      GridConfig(cols = cols, rows = rows, parts = parts.toArray)
    }
  }

  /** Build from column definitions: each column has a width and a list of cell heights (variable count per column). */
  def fromColumnDefs(colDefs: List[ColumnDef]): GridConfig = {
    if (colDefs.isEmpty) GridConfig(0, 0, Array.empty)
    else {
      val rows = colDefs.map(_.cellHeights.length).max
      val cols = colDefs.length
      val (_, parts) = colDefs.foldLeft((0, Vector.empty[GridPart])) { case ((x, acc), col) =>
        val (_, colParts) = col.cellHeights.foldLeft((0, Vector.empty[GridPart])) { case ((y, pacc), h) =>
          (y + h, pacc :+ GridPart(x = x, y = y, width = col.width, height = h))
        }
        (x + col.width, acc ++ colParts)
      }
      GridConfig(cols = cols, rows = rows, parts = parts.toArray)
    }
  }
}