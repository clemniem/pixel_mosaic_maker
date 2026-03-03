package clemniem.screens

import clemniem.StoredEntity
import munit.FunSuite

/** Tests for Gallery update helpers — pure function tests (model + action -> model).
  * No browser/DOM required since these are all pure state transformations.
  */
class GallerySpec extends FunSuite {

  /** Minimal entity for testing. */
  private case class TestEntity(id: String, name: String) extends StoredEntity

  private val pageSize = 3

  // ---- initState ----

  test("initState: items None, page 1, no pending delete") {
    val state = Gallery.initState[TestEntity]
    assertEquals(state.items, None)
    assertEquals(state.currentPage, 1)
    assertEquals(state.pendingDeleteId, None)
  }

  // ---- onLoaded ----

  test("onLoaded: stores items and stays on page 1") {
    val state = Gallery.initState[TestEntity]
    val items = List(TestEntity("1", "A"), TestEntity("2", "B"))
    val next  = Gallery.onLoaded(state, items, pageSize)
    assertEquals(next.items, Some(items))
    assertEquals(next.currentPage, 1)
  }

  test("onLoaded: empty list -> Some(Nil), page 1") {
    val state = Gallery.initState[TestEntity]
    val next  = Gallery.onLoaded(state, Nil, pageSize)
    assertEquals(next.items, Some(Nil))
    assertEquals(next.currentPage, 1)
  }

  test("onLoaded: clamps page when current page exceeds total") {
    val state = Gallery.initState[TestEntity].copy(currentPage = 5)
    val items = List(TestEntity("1", "A"))
    val next  = Gallery.onLoaded(state, items, pageSize)
    assertEquals(next.currentPage, 1)
  }

  test("onLoaded: preserves valid page") {
    val state = Gallery.initState[TestEntity].copy(currentPage = 2)
    val items = (1 to 6).map(i => TestEntity(i.toString, s"Item $i")).toList
    val next  = Gallery.onLoaded(state, items, pageSize)
    assertEquals(next.currentPage, 2)
  }

  // ---- onRequestDelete ----

  test("onRequestDelete: sets pendingDeleteId") {
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], List(TestEntity("1", "A")), pageSize)
    val next  = Gallery.onRequestDelete(state, "1")
    assertEquals(next.pendingDeleteId, Some("1"))
  }

  test("onRequestDelete: replaces previous pendingDeleteId") {
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], List(TestEntity("1", "A"), TestEntity("2", "B")), pageSize)
    val step1 = Gallery.onRequestDelete(state, "1")
    val step2 = Gallery.onRequestDelete(step1, "2")
    assertEquals(step2.pendingDeleteId, Some("2"))
  }

  // ---- onCancelDelete ----

  test("onCancelDelete: clears pendingDeleteId") {
    val state   = Gallery.onLoaded(Gallery.initState[TestEntity], List(TestEntity("1", "A")), pageSize)
    val pending = Gallery.onRequestDelete(state, "1")
    val next    = Gallery.onCancelDelete(pending)
    assertEquals(next.pendingDeleteId, None)
  }

  // ---- onPreviousPage ----

  test("onPreviousPage: decrements page") {
    val state = Gallery.initState[TestEntity].copy(currentPage = 3)
    val next  = Gallery.onPreviousPage(state)
    assertEquals(next.currentPage, 2)
  }

  test("onPreviousPage: does not go below 1") {
    val state = Gallery.initState[TestEntity].copy(currentPage = 1)
    val next  = Gallery.onPreviousPage(state)
    assertEquals(next.currentPage, 1)
  }

  test("onPreviousPage: from page 0 (invalid) stays at 1") {
    val state = Gallery.initState[TestEntity].copy(currentPage = 0)
    val next  = Gallery.onPreviousPage(state)
    assertEquals(next.currentPage, 1)
  }

  // ---- onNextPage ----

  test("onNextPage: increments page when more pages available") {
    val items = (1 to 7).map(i => TestEntity(i.toString, s"Item $i")).toList
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], items, pageSize)
    val next  = Gallery.onNextPage(state, pageSize)
    assertEquals(next.currentPage, 2)
  }

  test("onNextPage: does not exceed total pages") {
    val items = (1 to 7).map(i => TestEntity(i.toString, s"Item $i")).toList
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], items, pageSize).copy(currentPage = 3)
    val next  = Gallery.onNextPage(state, pageSize)
    assertEquals(next.currentPage, 3)
  }

  test("onNextPage: empty list stays at page 1") {
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], Nil, pageSize)
    val next  = Gallery.onNextPage(state, pageSize)
    assertEquals(next.currentPage, 1)
  }

  test("onNextPage: items not loaded stays at page 1") {
    val state = Gallery.initState[TestEntity]
    val next  = Gallery.onNextPage(state, pageSize)
    assertEquals(next.currentPage, 1)
  }

  // ---- pagination round-trip ----

  test("pagination: navigate forward and backward through pages") {
    val items = (1 to 10).map(i => TestEntity(i.toString, s"Item $i")).toList
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], items, pageSize)
    assertEquals(state.currentPage, 1)

    val p2 = Gallery.onNextPage(state, pageSize)
    assertEquals(p2.currentPage, 2)

    val p3 = Gallery.onNextPage(p2, pageSize)
    assertEquals(p3.currentPage, 3)

    val p4 = Gallery.onNextPage(p3, pageSize)
    assertEquals(p4.currentPage, 4)

    val p5 = Gallery.onNextPage(p4, pageSize)
    assertEquals(p5.currentPage, 4) // 10 items / 3 per page = 4 pages max

    val back = Gallery.onPreviousPage(p4)
    assertEquals(back.currentPage, 3)
  }

  // ---- state independence ----

  test("onLoaded preserves pendingDeleteId") {
    val state   = Gallery.initState[TestEntity]
    val pending = Gallery.onRequestDelete(state, "keep-me")
    val loaded  = Gallery.onLoaded(pending, List(TestEntity("1", "A")), pageSize)
    assertEquals(loaded.pendingDeleteId, Some("keep-me"))
  }

  test("onNextPage preserves pendingDeleteId") {
    val items = (1 to 7).map(i => TestEntity(i.toString, s"Item $i")).toList
    val state = Gallery.onLoaded(Gallery.initState[TestEntity], items, pageSize)
    val withDelete = Gallery.onRequestDelete(state, "3")
    val next = Gallery.onNextPage(withDelete, pageSize)
    assertEquals(next.pendingDeleteId, Some("3"))
    assertEquals(next.currentPage, 2)
  }
}
