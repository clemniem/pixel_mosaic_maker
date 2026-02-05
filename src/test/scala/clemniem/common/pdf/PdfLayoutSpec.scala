package clemniem.common.pdf

import munit.FunSuite

class PdfLayoutSpec extends FunSuite {

  test("coverInstructions contains exactly one PageSize") {
    val list = PdfLayout.coverInstructions("My Mosaic")
    val pageSizes = list.collect { case Instruction.PageSize(_, _) => () }
    assertEquals(pageSizes.length, 1)
  }

  test("coverInstructions contains at least one Text with the title") {
    val title = "My Mosaic"
    val list  = PdfLayout.coverInstructions(title)
    val texts = list.collect { case Instruction.Text(_, _, value) => value }
    assert(texts.exists(_.contains(title)), clues(texts))
  }

  test("coverInstructions does not contain Save") {
    val list = PdfLayout.coverInstructions("X")
    assert(!list.exists { case Instruction.Save(_) => true; case _ => false })
  }
}
