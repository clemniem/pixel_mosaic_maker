package clemniem

/** Registry of all screens and the initial screen for the SPA. */
final case class ScreenRegistry(
    screens: List[Screen],
    initialScreenId: ScreenId
) {
  def screenFor(id: ScreenId): Option[Screen] =
    screens.find(_.screenId == id)
}
