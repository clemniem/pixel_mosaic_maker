package mosaic.domain

final case class GridSize(width: Int, height: Int)

final case class GridConfig(
  baseSize: GridSize,
  columns: Vector[Int],
  rows: Vector[Int]
)

final case class Rect(
  x: Int,
  y: Int,
  width: Int,
  height: Int,
  row: Int,
  col: Int
)

object GridLayout {
  def rectangles(grid: GridConfig): Vector[Rect] = {
    val xs = grid.columns.scanLeft(0)(_ + _).dropRight(1)
    val ys = grid.rows.scanLeft(0)(_ + _).dropRight(1)

    for {
      (x, col) <- xs.zipWithIndex.toVector
      (y, row) <- ys.zipWithIndex.toVector
    } yield Rect(
      x,
      y,
      grid.columns(col),
      grid.rows(row),
      row,
      col
    )
  }
}
