package clemniem

import cats.effect.IO
import tyrian.{Cmd, Html, Sub}

/** Identifies a screen in the SPA. See FLOW.md for the six-step flow. */
trait ScreenId {
  def name: String
}

/** All screen IDs for the app flow (GridConfig → ImageUpload → Palettes → BuildConfig → Build → Print). */
object ScreenId {
  case object GridConfigId       extends ScreenId { val name = "grid-config" }
  case object ImageUploadId      extends ScreenId { val name = "image-upload" }
  case object PalettesId         extends ScreenId { val name = "palettes" }
  case object BuildConfigId      extends ScreenId { val name = "build-config" }
  case object BuildId            extends ScreenId { val name = "build" }
  case object PrintInstructionsId extends ScreenId { val name = "print-instructions" }
}

/** Data passed from one screen to the next when navigating. See FLOW.md for the six-step flow. */
sealed trait ScreenOutput

object ScreenOutput {
  /** Step 1 → next: chosen grid of plates. */
  final case class GridConfigDone(grid: GridConfig) extends ScreenOutput

  /** Step 2 → next: uploaded and prepped pixel image (pixel-true, palette-reduced). */
  final case class ImageUploaded(name: String) extends ScreenOutput
  // TODO: add pixelImage: PixelImage when type exists (see gbcamutil PixelPic)

  /** Step 3 → next: chosen palette combination. */
  final case class PaletteChosen(paletteId: String) extends ScreenOutput
  // TODO: add palette type when Palettes screen exists (see gbcamutil ChangePalette / LegoColor)

  /** Step 4 → next: build = Palette + GridConfig + Image + Offset (stateless). */
  final case class BuildConfigDone(config: BuildConfig) extends ScreenOutput

  /** Step 5 → Print: same config, for PDF generation. */
  final case class BuildStarted(config: BuildConfig) extends ScreenOutput
}

/** Stateless build definition: Palette + GridConfig + Image + Offset. */
final case class BuildConfig(
    grid: GridConfig,
    imageRef: String,
    paletteRef: String,
    offsetX: Int,
    offsetY: Int
)

/** Root-level messages: navigation or delegation to the current screen. */
sealed trait RootMsg
object RootMsg {
  case class NavigateTo(screenId: ScreenId, output: Option[ScreenOutput]) extends RootMsg
  case class HandleScreenMsg(screenId: ScreenId, msg: Any)               extends RootMsg
}

/**
 * Message a screen can emit to request navigation to another screen with optional output.
 * Include this in your screen's Msg type (e.g. `enum MyMsg { case A; case B; case GoNext(s: ScreenId, o: Option[ScreenOutput]) }`)
 * or use [[NavigateNext]] directly so the root app can handle it and switch screens.
 */
case class NavigateNext(screenId: ScreenId, output: Option[ScreenOutput])

/**
 * Abstraction for a single screen in the SPA.
 * Each screen has its own Model, Msg, init, update, view, and subscriptions.
 * Use [[wrapMsg]] to lift screen messages to [[RootMsg]] so the root app can run [[Cmd]]/[[Sub]].
 *
 * To add a new screen:
 *  1. Define a [[ScreenId]] case object (e.g. in the same file as the screen).
 *  2. Extend [[ScreenOutput]] with a case class for the data this screen receives from the previous step (if any).
 *  3. Implement a [[Screen]] (object or class) with init/update/view/subscriptions.
 *  4. Register the screen in [[ScreenRegistry]] and set [[ScreenRegistry.initialScreenId]] or navigate via [[RootMsg.NavigateTo]].
 *
 * To navigate to the next screen from within a screen: include [[NavigateNext]] in your screen's Msg type
 * and emit `Cmd.Emit(NavigateNext(nextScreenId, Some(yourOutput)))`. The root app will run the next screen's
 * `init(Some(yourOutput))`. You can also emit [[RootMsg.NavigateTo]] from outside (e.g. router) if needed.
 */
trait Screen {
  type Model
  type Msg

  /** Unique identifier for this screen (use this, not `id`, to avoid shadowing HTML's `id` attribute). */
  val screenId: ScreenId

  /** Initial state and optional command. `previous` is the output from the previous screen, if any. */
  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg])

  /** Update state and optional command in response to a message. */
  def update(model: Model): Msg => (Model, Cmd[IO, Msg])

  /** View for this screen. Root app will map [[Msg]] to [[RootMsg]] via [[wrapMsg]]. */
  def view(model: Model): Html[Msg]

  /** Optional subscriptions. Default: none. */
  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

  /** Lifts a screen message to [[RootMsg]] so the root app can dispatch it. */
  def wrapMsg(msg: Msg): RootMsg = RootMsg.HandleScreenMsg(screenId, msg)
}
