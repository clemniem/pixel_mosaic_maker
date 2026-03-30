package clemniem.common

import clemniem.common.pdf.Instruction
import clemniem.{Layout, Pixel, PixelPic}
import munit.FunSuite

class PdfUtilsSpec extends FunSuite {

  private val black = Pixel(0, 0, 0, 255)

  private def singleSectionPic: PixelPic =
    PixelPic(
      width = 16,
      height = 16,
      paletteLookup = Vector(black),
      pixels = Vector.fill(16 * 16)(0),
      pixelCounts = Map(0 -> (16 * 16)),
      name = "single-section"
    ).getOrElse(fail("expected valid test picture"))

  test("previewBookPages keeps every preview page for cover-only requests") {
    val preview = PdfUtils.previewBookPages(PrintBookRequest(title = "Cover only", mosaicPicAndGridOpt = None))

    assertEquals(preview.totalPages, 2)
    assertEquals(preview.pages.length, 2)
    assert(preview.pages.head.exists { case Instruction.PageSize(_, _) => true; case _ => false })
  }

  test("previewBookPages does not truncate multi-page mosaic previews") {
    val request = PrintBookRequest(
      title = "Mosaic",
      mosaicPicAndGridOpt = Some(singleSectionPic -> Layout.make(Seq(16), Seq(16)))
    )

    val preview = PdfUtils.previewBookPages(request)

    assert(preview.pages.length > 5, clues(preview.pages.length))
    assertEquals(preview.pages.length, preview.totalPages)
  }
}
