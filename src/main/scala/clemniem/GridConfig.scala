package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class GridPart(x: Int, y: Int, width: Int, height: Int)

object GridPart {
  given Encoder[GridPart] = deriveEncoder
  given Decoder[GridPart] = deriveDecoder
}

final case class GridConfig(cols: Int, rows: Int, parts: Array[GridPart]) {
//  require(
//    parts.length == cols * rows,
//    s"Expected ${cols * rows} grid parts, but got ${parts.length}"
//  )

  val width: Int = {
    (0 until cols).map { col =>
      parts(col).width
    }.sum
  }

  val height: Int = {
    (0 until rows).map { row =>
      parts(row * cols).height
    }.sum
  }

  def partAt(col: Int, row: Int): GridPart = {
    parts(row * cols + col)
  }
}

object GridConfig {
  given Encoder[GridConfig] = deriveEncoder
  given Decoder[GridConfig] = deriveDecoder

  def make(rows: Seq[Int], columns: Seq[Int]): GridConfig = {

    val rowOffsets: Seq[Int] =
      rows.scanLeft(0)(_ + _).dropRight(1)

    val colOffsets: Seq[Int] =
      columns.scanLeft(0)(_ + _).dropRight(1)

    val parts: Array[GridPart] =
      (for {
        (rowHeight, y) <- rows.zip(rowOffsets)
        (colWidth, x) <- columns.zip(colOffsets)
      } yield GridPart(
        x = x,
        y = y,
        width = colWidth,
        height = rowHeight
      )).toArray

    GridConfig(
      cols = columns.length,
      rows = rows.length,
      parts = parts
    )
  }
}