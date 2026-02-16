package clemniem

/** Registry of all screens and the initial screen for the SPA. */
final case class ScreenRegistry(
  screens: List[Screen],
  initialScreenId: ScreenId) {
  private val screenMap: Map[ScreenId, Screen] = screens.map(s => s.screenId -> s).toMap

  def screenFor(id: ScreenId): Option[Screen] =
    screenMap.get(id)
}
