package clemniem.common

import munit.FunSuite

class PaginationSpec extends FunSuite {

  // ---- totalPagesFor ----

  test("totalPagesFor: empty list -> 1 page") {
    assertEquals(Pagination.totalPagesFor(0, 3), 1)
  }

  test("totalPagesFor: fewer items than page size -> 1 page") {
    assertEquals(Pagination.totalPagesFor(2, 3), 1)
  }

  test("totalPagesFor: exactly page size -> 1 page") {
    assertEquals(Pagination.totalPagesFor(3, 3), 1)
  }

  test("totalPagesFor: one more than page size -> 2 pages") {
    assertEquals(Pagination.totalPagesFor(4, 3), 2)
  }

  test("totalPagesFor: exactly two pages -> 2 pages") {
    assertEquals(Pagination.totalPagesFor(6, 3), 2)
  }

  test("totalPagesFor: 7 items with page size 3 -> 3 pages") {
    assertEquals(Pagination.totalPagesFor(7, 3), 3)
  }

  test("totalPagesFor: page size 1 -> size pages") {
    assertEquals(Pagination.totalPagesFor(5, 1), 5)
  }

  test("totalPagesFor: negative size -> 1 page") {
    assertEquals(Pagination.totalPagesFor(-1, 3), 1)
  }

  // ---- clampPage ----

  test("clampPage: valid page stays unchanged") {
    assertEquals(Pagination.clampPage(2, 3), 2)
  }

  test("clampPage: page below 1 clamps to 1") {
    assertEquals(Pagination.clampPage(0, 3), 1)
    assertEquals(Pagination.clampPage(-5, 3), 1)
  }

  test("clampPage: page above total clamps to total") {
    assertEquals(Pagination.clampPage(5, 3), 3)
  }

  test("clampPage: page 1, total 1 -> 1") {
    assertEquals(Pagination.clampPage(1, 1), 1)
  }

  // ---- sliceForPage ----

  test("sliceForPage: first page of multi-page list") {
    val items                = List("a", "b", "c", "d", "e")
    val (slice, page, total) = Pagination.sliceForPage(items, 1, 3)
    assertEquals(slice, List("a", "b", "c"))
    assertEquals(page, 1)
    assertEquals(total, 2)
  }

  test("sliceForPage: second page of multi-page list") {
    val items                = List("a", "b", "c", "d", "e")
    val (slice, page, total) = Pagination.sliceForPage(items, 2, 3)
    assertEquals(slice, List("d", "e"))
    assertEquals(page, 2)
    assertEquals(total, 2)
  }

  test("sliceForPage: page beyond total clamps to last page") {
    val items                = List("a", "b", "c", "d", "e")
    val (slice, page, total) = Pagination.sliceForPage(items, 10, 3)
    assertEquals(slice, List("d", "e"))
    assertEquals(page, 2)
    assertEquals(total, 2)
  }

  test("sliceForPage: empty list returns empty slice, page 1 of 1") {
    val (slice, page, total) = Pagination.sliceForPage(List.empty[Int], 1, 3)
    assertEquals(slice, Nil)
    assertEquals(page, 1)
    assertEquals(total, 1)
  }

  test("sliceForPage: exactly one page") {
    val items                = List(1, 2, 3)
    val (slice, page, total) = Pagination.sliceForPage(items, 1, 3)
    assertEquals(slice, List(1, 2, 3))
    assertEquals(page, 1)
    assertEquals(total, 1)
  }
}
