package clemniem.screens

import clemniem.ScreenId

/** Application-level workflow config: which screens appear on the overview page, their descriptions, and the "Next →"
  * navigation order. Separated from [[ScreenId]] (framework) so the enum stays generic.
  */
object ScreenFlow {

  /** Screen IDs shown on the overview page as link cards, in order. */
  val overviewScreenIds: List[ScreenId] =
    List(
      ScreenId.ImagesId,
      ScreenId.PalettesId,
      ScreenId.LayoutsId,
      ScreenId.BuildConfigsId,
      ScreenId.BuildsId,
      ScreenId.PrintConfigsId)

  /** Short description for the overview page link card. None = screen is not shown on overview. */
  def overviewDescription(id: ScreenId): Option[String] = id match {
    case ScreenId.LayoutsId      => Some("How your mosaic is split into sections")
    case ScreenId.PalettesId     => Some("Color palettes")
    case ScreenId.ImagesId       => Some("Upload and manage your images")
    case ScreenId.BuildConfigsId => Some("Choose layout, image and colors")
    case ScreenId.BuildsId       => Some("Step-by-step building instructions")
    case ScreenId.PrintConfigsId => Some("Create a printable PDF guide")
    case _                       => None
  }

  /** Next screen in the overview flow (for the "Next →" button). Overview → first in list; last → Overview; editors →
    * next after their gallery.
    */
  def nextInOverviewOrder(current: ScreenId): ScreenId =
    if (current == ScreenId.OverviewId) overviewScreenIds.head
    else {
      val idx = overviewScreenIds.indexOf(current)
      if (idx >= 0) {
        if (idx + 1 < overviewScreenIds.length) overviewScreenIds(idx + 1)
        else ScreenId.OverviewId
      } else
        current match {
          case ScreenId.LayoutId      => ScreenId.PalettesId
          case ScreenId.PaletteId     => ScreenId.LayoutsId
          case ScreenId.ImageUploadId => ScreenId.BuildConfigsId
          case ScreenId.BuildConfigId => ScreenId.BuildsId
          case ScreenId.BuildId       => ScreenId.PrintConfigsId
          case ScreenId.AboutId       => ScreenId.OverviewId
          case _                      => ScreenId.OverviewId
        }
    }
}
