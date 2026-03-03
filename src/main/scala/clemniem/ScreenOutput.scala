package clemniem

/** Application-level data passed from one screen to the next when navigating.
  * See docs/FLOW.md for the six-step flow.
  *
  * This is an application concern, not a framework concern — the [[Screen]] trait and [[RootMsg]]
  * use `Option[Any]` so the framework has no dependency on these types. Each screen pattern-matches
  * on the expected variant in its `init`.
  */
sealed trait ScreenOutput

object ScreenOutput {

  /** Step 1 → next: chosen layout of sections. */
  final case class LayoutDone(grid: Layout) extends ScreenOutput

  /** Step 2 → next: uploaded and prepped pixel image (pixel-true, palette-reduced). */
  final case class ImageUploaded(name: String) extends ScreenOutput

  /** Step 3 → next: chosen palette combination. */
  final case class PaletteChosen(paletteId: String) extends ScreenOutput

  /** Step 4 → next: build = Palette + Layout + Image + Offset (stateless). */
  final case class BuildConfigDone(config: BuildConfig) extends ScreenOutput

  /** Step 5 → Print: same config, for PDF generation. */
  final case class BuildStarted(config: BuildConfig) extends ScreenOutput

  /** Open layout editor to edit an existing stored layout. */
  final case class EditLayout(stored: StoredLayout) extends ScreenOutput

  /** Open palette editor to edit an existing stored palette. */
  final case class EditPalette(stored: StoredPalette) extends ScreenOutput

  /** Open palette editor with colors from an image (e.g. from Images gallery) so the user can save as a palette. */
  final case class NewPaletteFromImage(name: String, colors: Vector[Color]) extends ScreenOutput

  /** Open build config editor to edit an existing stored config. */
  final case class EditBuildConfig(stored: StoredBuildConfig) extends ScreenOutput

  /** Start the step-by-step build with the selected build config (sections → 16×16 cells). */
  final case class StartBuild(stored: StoredBuildConfig) extends ScreenOutput

  /** Resume a build from the builds list (has buildConfigRef + savedStepIndex). */
  final case class ResumeBuild(stored: StoredBuild) extends ScreenOutput

  /** Open the print instructions screen with a previously saved print config. */
  final case class OpenPrintConfig(stored: StoredPrintConfig) extends ScreenOutput
}
