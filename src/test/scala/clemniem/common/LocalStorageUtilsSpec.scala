package clemniem.common

import clemniem.{GridConfig, GridPart}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

/** Unit tests for the **serialization logic** used by [[LocalStorageUtils]].
  *
  * LocalStorageUtils itself uses the browser's LocalStorage (via Tyrian Cmd), which is not
  * available in the Node.js test environment. So we don't test the Cmd/getItem/setItem wiring.
  *
  * We do test the pure part: the same encode/decode path that LocalStorageUtils uses
  * (circe [[Encoder]]/[[Decoder]] + [[io.circe.parser.decode]]) so that:
  * - Round-trips (encode then decode) preserve values.
  * - Invalid JSON produces a decode error (Left).
  * - List round-trips work for stored collections (e.g. multiple grid configs).
  */
class LocalStorageUtilsSpec extends FunSuite {

  test("GridPart round-trips through JSON (same format as LocalStorageUtils)") {
    val part = GridPart(x = 10, y = 20, width = 48, height = 16)
    val json = part.asJson.noSpacesSortKeys
    val decoded = decode[GridPart](json)
    assert(decoded.isRight)
    assertEquals(decoded.toOption.get, part)
  }

  test("GridConfig round-trips through JSON (same format as LocalStorageUtils)") {
    val config = GridConfig.make(List(48, 16, 48), List(48, 32, 48))
    val json = config.asJson.noSpacesSortKeys
    val decoded = decode[GridConfig](json)
    assert(decoded.isRight)
    val back = decoded.toOption.get
    assertEquals(back.cols, config.cols)
    assertEquals(back.rows, config.rows)
    assertEquals(back.parts.length, config.parts.length)
    back.parts.zip(config.parts).foreach { case (a, b) =>
      assertEquals(a.x, b.x)
      assertEquals(a.y, b.y)
      assertEquals(a.width, b.width)
      assertEquals(a.height, b.height)
    }
  }

  test("List[GridConfig] round-trips through JSON (saveList/loadList format)") {
    val config1 = GridConfig.make(List(48, 16), List(48, 32))
    val config2 = GridConfig.make(List(16, 16, 16), List(16, 16, 16))
    val list   = List(config1, config2)
    val json   = list.asJson.noSpacesSortKeys
    val decoded = decode[List[GridConfig]](json)
    assert(decoded.isRight)
    val back = decoded.toOption.get
    assertEquals(back.length, 2)
    assertEquals(back(0).cols, config1.cols)
    assertEquals(back(0).rows, config1.rows)
    assertEquals(back(1).cols, config2.cols)
    assertEquals(back(1).rows, config2.rows)
  }

  test("Empty list round-trips") {
    val list   = List.empty[GridConfig]
    val json   = list.asJson.noSpacesSortKeys
    val decoded = decode[List[GridConfig]](json)
    assert(decoded.isRight)
    assertEquals(decoded.toOption.get, Nil)
  }

  test("Invalid JSON for GridConfig returns Left with message") {
    val invalid = """{"cols": 2, "rows": 2}"""
    val decoded = decode[GridConfig](invalid)
    assert(decoded.isLeft)
    assert(decoded.left.toOption.get.getMessage.nonEmpty)
  }

  test("Invalid JSON for List[GridConfig] returns Left") {
    val invalid = """not json"""
    val decoded = decode[List[GridConfig]](invalid)
    assert(decoded.isLeft)
  }
}
