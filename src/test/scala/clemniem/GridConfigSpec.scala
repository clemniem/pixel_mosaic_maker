package clemniem

import munit.FunSuite

class GridConfigSpec extends FunSuite {

  test("fromRowDefs: empty list gives empty grid") {
    val g = GridConfig.fromRowDefs(Nil)
    assertEquals(g.parts.length, 0)
    assertEquals(g.width, 0)
    assertEquals(g.height, 0)
  }

  test("fromRowDefs: one row one cell") {
    val g = GridConfig.fromRowDefs(List(RowDef(16, List(48))))
    assertEquals(g.parts.length, 1)
    assertEquals(g.parts(0), GridPart(0, 0, 48, 16))
    assertEquals(g.width, 48)
    assertEquals(g.height, 16)
  }

  test("fromRowDefs: two rows with variable cells per row") {
    val g = GridConfig.fromRowDefs(
      List(
        RowDef(16, List(48, 32)),
        RowDef(32, List(80))
      )
    )
    assertEquals(g.parts.length, 3)
    assertEquals(g.parts(0), GridPart(0, 0, 48, 16))
    assertEquals(g.parts(1), GridPart(48, 0, 32, 16))
    assertEquals(g.parts(2), GridPart(0, 16, 80, 32))
    assertEquals(g.width, 80)
    assertEquals(g.height, 48)
  }

  test("fromColumnDefs: empty list gives empty grid") {
    val g = GridConfig.fromColumnDefs(Nil)
    assertEquals(g.parts.length, 0)
    assertEquals(g.width, 0)
    assertEquals(g.height, 0)
  }

  test("fromColumnDefs: one column one cell") {
    val g = GridConfig.fromColumnDefs(List(ColumnDef(48, List(16))))
    assertEquals(g.parts.length, 1)
    assertEquals(g.parts(0), GridPart(0, 0, 48, 16))
    assertEquals(g.width, 48)
    assertEquals(g.height, 16)
  }

  test("fromColumnDefs: two columns with variable cells per column") {
    val g = GridConfig.fromColumnDefs(
      List(
        ColumnDef(48, List(16, 16)),
        ColumnDef(32, List(32))
      )
    )
    assertEquals(g.parts.length, 3)
    assertEquals(g.parts(0), GridPart(0, 0, 48, 16))
    assertEquals(g.parts(1), GridPart(0, 16, 48, 16))
    assertEquals(g.parts(2), GridPart(48, 0, 32, 32))
    assertEquals(g.width, 80)
    assertEquals(g.height, 32)
  }

  test("make (uniform grid) still works and width/height from bounding box") {
    val g = GridConfig.make(List(48, 16, 48), List(48, 32, 48))
    assertEquals(g.cols, 3)
    assertEquals(g.rows, 3)
    assertEquals(g.parts.length, 9)
    assertEquals(g.width, 48 + 32 + 48)
    assertEquals(g.height, 48 + 16 + 48)
  }

  test("RowDef.normalizeToRectangle: pads rows so all have same total width") {
    val rowDefs = List(
      RowDef(16, List(48, 32)),
      RowDef(32, List(40))
    )
    val norm = RowDef.normalizeToRectangle(rowDefs)
    assertEquals(norm(0).cellWidths, List(48, 32))
    assertEquals(norm(0).totalWidth, 80)
    assertEquals(norm(1).cellWidths, List(40, 40))
    assertEquals(norm(1).totalWidth, 80)
    val g = GridConfig.fromRowDefs(norm)
    assertEquals(g.width, 80)
    assertEquals(g.height, 48)
    assertEquals(g.parts.length, 4)
  }

  test("ColumnDef.normalizeToRectangle: pads columns so all have same total height") {
    val colDefs = List(
      ColumnDef(48, List(16, 16)),
      ColumnDef(32, List(24))
    )
    val norm = ColumnDef.normalizeToRectangle(colDefs)
    assertEquals(norm(0).cellHeights, List(16, 16))
    assertEquals(norm(0).totalHeight, 32)
    assertEquals(norm(1).cellHeights, List(24, 8))
    assertEquals(norm(1).totalHeight, 32)
    val g = GridConfig.fromColumnDefs(norm)
    assertEquals(g.width, 80)
    assertEquals(g.height, 32)
    assertEquals(g.parts.length, 4)
  }
}
