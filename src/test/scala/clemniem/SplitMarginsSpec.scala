package clemniem

import clemniem.common.{PdfUtils, PrintBookRequest}
import clemniem.common.pdf.{Instruction, PdfLayout, PdfLayoutConfig}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class SplitMarginsSpec extends FunSuite {

  test("PdfUtils default side margin is 6.0") {
    assertEquals(PdfUtils.defaultSideMarginMm, 6.0)
  }

  test("PdfUtils default top/bottom margin is 5.0") {
    assertEquals(PdfUtils.defaultTopBottomMarginMm, 5.0)
  }

  test("PrintBookRequest uses split margin defaults") {
    val req = PrintBookRequest(title = "Test", mosaicPicAndGridOpt = None)
    assertEquals(req.sideMarginMm, 6.0)
    assertEquals(req.topBottomMarginMm, 5.0)
  }

  test("StoredPrintConfig decodes old JSON with single printerMarginMm") {
    val oldJson =
      """{
        |  "id": "old-1",
        |  "name": "Old Config",
        |  "selectedBuildId": null,
        |  "selectedBuildConfigId": null,
        |  "title": "Old Title",
        |  "stepSizePx": 16,
        |  "pageBackgroundColorHex": "#fdfbe6",
        |  "patchBackgroundColorHex": "#cccccc",
        |  "stacked": false,
        |  "printerMarginMm": 8.0,
        |  "contentTopOffsetMm": 2.0,
        |  "innerMargin": false
        |}""".stripMargin
    val result = decode[StoredPrintConfig](oldJson)
    assert(result.isRight, clues(result))
    val cfg = result.toOption.get
    assertEquals(cfg.sideMarginMm, 8.0)
    assertEquals(cfg.topBottomMarginMm, 8.0)
  }

  test("StoredPrintConfig decodes new JSON with split margins") {
    val newJson =
      """{
        |  "id": "new-1",
        |  "name": "New Config",
        |  "selectedBuildId": null,
        |  "selectedBuildConfigId": null,
        |  "title": "New Title",
        |  "stepSizePx": 16,
        |  "pageBackgroundColorHex": "#fdfbe6",
        |  "patchBackgroundColorHex": "#cccccc",
        |  "stacked": false,
        |  "sideMarginMm": 7.0,
        |  "topBottomMarginMm": 4.0,
        |  "contentTopOffsetMm": 2.0,
        |  "innerMargin": true
        |}""".stripMargin
    val result = decode[StoredPrintConfig](newJson)
    assert(result.isRight, clues(result))
    val cfg = result.toOption.get
    assertEquals(cfg.sideMarginMm, 7.0)
    assertEquals(cfg.topBottomMarginMm, 4.0)
    assertEquals(cfg.innerMargin, true)
  }

  test("StoredPrintConfig decodes JSON with neither old nor new margin fields using defaults") {
    val minimalJson =
      """{
        |  "id": "min-1",
        |  "name": "Minimal",
        |  "selectedBuildId": null,
        |  "selectedBuildConfigId": null,
        |  "title": "Minimal",
        |  "stepSizePx": 16,
        |  "pageBackgroundColorHex": "#fdfbe6",
        |  "patchBackgroundColorHex": "#cccccc",
        |  "stacked": false,
        |  "contentTopOffsetMm": 2.0
        |}""".stripMargin
    val result = decode[StoredPrintConfig](minimalJson)
    assert(result.isRight, clues(result))
    val cfg = result.toOption.get
    assertEquals(cfg.sideMarginMm, 6.0)
    assertEquals(cfg.topBottomMarginMm, 5.0)
  }

  test("StoredPrintConfig round-trips through JSON with split margins") {
    val original = StoredPrintConfig(
      id = "rt-1",
      name = "Round Trip",
      selectedBuildId = Some("b1"),
      selectedBuildConfigId = Some("bc1"),
      title = "RT Title",
      stepSizePx = 20,
      pageBackgroundColorHex = "#ffffff",
      patchBackgroundColorHex = "#000000",
      stacked = true,
      sideMarginMm = 10.0,
      topBottomMarginMm = 3.0,
      contentTopOffsetMm = 1.5,
      innerMargin = true
    )
    val json    = original.asJson.noSpacesSortKeys
    val decoded = decode[StoredPrintConfig](json)
    assert(decoded.isRight, clues(decoded))
    assertEquals(decoded.toOption.get, original)
  }

  test("coverInstructions offsets title X by side margin and Y by top/bottom margin") {
    val config    = PdfLayoutConfig.default
    val side      = 6.0
    val topBottom = 5.0
    val instrs    = PdfLayout.coverInstructions("Hi", side, topBottom, config)
    val texts     = instrs.collect { case Instruction.Text(x, y, _) => (x, y) }
    assert(texts.nonEmpty)
    val (tx, ty) = texts.head
    assertEquals(tx, config.cover.titleOnlyXMm + side)
    assertEquals(ty, config.cover.titleOnlyYMm + topBottom)
  }

  test("BookPreview carries separate side and top/bottom margins") {
    val preview = PdfUtils.previewBookPages(PrintBookRequest(
      title = "Margin Test",
      mosaicPicAndGridOpt = None,
      sideMarginMm = 7.0,
      topBottomMarginMm = 3.0
    ))
    assertEquals(preview.sideMarginMm, 7.0)
    assertEquals(preview.topBottomMarginMm, 3.0)
  }

  test("preview page count unchanged after margin split for cover-only request") {
    val preview = PdfUtils.previewBookPages(PrintBookRequest(title = "Cover Only", mosaicPicAndGridOpt = None))
    assertEquals(preview.totalPages, 2)
    assertEquals(preview.pages.length, 2)
  }

  test("preview page count unchanged after margin split for mosaic request") {
    val black = Pixel(0, 0, 0, 255)
    val pic = PixelPic(
      width = 16,
      height = 16,
      paletteLookup = Vector(black),
      pixels = Vector.fill(16 * 16)(0),
      pixelCounts = Map(0 -> (16 * 16)),
      name = "test-pic"
    ).getOrElse(fail("expected valid test picture"))
    val grid = Layout.make(Seq(16), Seq(16))

    val previewOld = PdfUtils.previewBookPages(PrintBookRequest(
      title = "Mosaic",
      mosaicPicAndGridOpt = Some(pic -> grid),
      sideMarginMm = 6.0,
      topBottomMarginMm = 6.0
    ))
    val previewNew = PdfUtils.previewBookPages(PrintBookRequest(
      title = "Mosaic",
      mosaicPicAndGridOpt = Some(pic -> grid),
      sideMarginMm = 6.0,
      topBottomMarginMm = 5.0
    ))
    assertEquals(previewOld.totalPages, previewNew.totalPages)
  }
}
