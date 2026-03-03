package clemniem.screens

import clemniem.ScreenId
import munit.FunSuite

/** Tests for ScreenFlow navigation logic — pure function tests (ScreenId -> ScreenId). */
class ScreenFlowSpec extends FunSuite {

  // ---- overviewScreenIds ----

  test("overviewScreenIds: contains all 6 gallery screen IDs") {
    assertEquals(ScreenFlow.overviewScreenIds.length, 6)
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.ImagesId))
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.PalettesId))
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.LayoutsId))
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.BuildConfigsId))
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.BuildsId))
    assert(ScreenFlow.overviewScreenIds.contains(ScreenId.PrintConfigsId))
  }

  test("overviewScreenIds: does not contain editor or detail screens") {
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.OverviewId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.LayoutId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.PaletteId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.ImageUploadId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.BuildConfigId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.BuildId))
    assert(!ScreenFlow.overviewScreenIds.contains(ScreenId.AboutId))
  }

  // ---- overviewDescription ----

  test("overviewDescription: all overview screens have a description") {
    ScreenFlow.overviewScreenIds.foreach { id =>
      assert(ScreenFlow.overviewDescription(id).isDefined, s"$id should have a description")
    }
  }

  test("overviewDescription: non-overview screens return None") {
    assertEquals(ScreenFlow.overviewDescription(ScreenId.OverviewId), None)
    assertEquals(ScreenFlow.overviewDescription(ScreenId.LayoutId), None)
    assertEquals(ScreenFlow.overviewDescription(ScreenId.PaletteId), None)
    assertEquals(ScreenFlow.overviewDescription(ScreenId.AboutId), None)
  }

  // ---- nextInOverviewOrder ----

  test("nextInOverviewOrder: Overview -> first overview screen") {
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.OverviewId), ScreenFlow.overviewScreenIds.head)
  }

  test("nextInOverviewOrder: gallery screens cycle through overview order") {
    val ids = ScreenFlow.overviewScreenIds
    ids.init.zip(ids.tail).foreach { case (current, expected) =>
      assertEquals(
        ScreenFlow.nextInOverviewOrder(current),
        expected,
        s"After $current should be $expected"
      )
    }
  }

  test("nextInOverviewOrder: last overview screen -> Overview") {
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenFlow.overviewScreenIds.last), ScreenId.OverviewId)
  }

  test("nextInOverviewOrder: editor screens go to next gallery") {
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.LayoutId), ScreenId.PalettesId)
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.PaletteId), ScreenId.LayoutsId)
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.ImageUploadId), ScreenId.BuildConfigsId)
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.BuildConfigId), ScreenId.BuildsId)
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.BuildId), ScreenId.PrintConfigsId)
  }

  test("nextInOverviewOrder: About -> Overview") {
    assertEquals(ScreenFlow.nextInOverviewOrder(ScreenId.AboutId), ScreenId.OverviewId)
  }

  // ---- full cycle ----

  test("nextInOverviewOrder: full overview cycle returns to Overview") {
    val ids = ScreenFlow.overviewScreenIds
    val visited = (0 until ids.length + 1).foldLeft(List(ScreenId.OverviewId)) { (acc, _) =>
      acc :+ ScreenFlow.nextInOverviewOrder(acc.last)
    }
    // Indices 1..6 should be the overview screens in order
    assertEquals(visited.slice(1, ids.length + 1), ids)
    // Index 7 should be back to Overview
    assertEquals(visited(ids.length + 1), ScreenId.OverviewId)
  }
}
