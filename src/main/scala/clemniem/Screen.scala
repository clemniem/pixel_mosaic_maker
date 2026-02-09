package clemniem

import cats.effect.IO
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import tyrian.{Cmd, Html, Sub}

/** Identifies a screen in the SPA. See docs/FLOW.md for the six-step flow. */
trait ScreenId {
  def name: String
  /** Display title used in the UI (e.g. header, document title, overview link card). */
  def title: String
  /** Short description for the overview page link card. None = screen is not shown on overview. */
  def overviewDescription: Option[String] = None
}

/** All screen IDs: Overview (home), galleries (list of saved items), and flow/editor screens. */
object ScreenId {
  case object OverviewId          extends ScreenId { val name = "overview";          val title = "Overview" }
  case object GridConfigsId       extends ScreenId { val name = "grid-configs";      val title = "Layout";             override val overviewDescription = Some("How your mosaic is split into sections") }
  case object GridConfigId        extends ScreenId { val name = "grid-config";       val title = "Edit layout" }
  case object PalettesId          extends ScreenId { val name = "palettes";           val title = "Palettes";           override val overviewDescription = Some("Color palettes") }
  case object PaletteId           extends ScreenId { val name = "palette";           val title = "Palette" }
  case object ImagesId            extends ScreenId { val name = "images";           val title = "Images";              override val overviewDescription = Some("Upload and manage your images") }
  case object BuildConfigsId      extends ScreenId { val name = "build-configs";     val title = "Mosaic setup";       override val overviewDescription = Some("Choose layout, image and colors") }
  case object BuildsId            extends ScreenId { val name = "builds";           val title = "Build";               override val overviewDescription = Some("Step-by-step building instructions") }
  case object ImageUploadId       extends ScreenId { val name = "image-upload";     val title = "Upload image" }
  case object BuildConfigId       extends ScreenId { val name = "build-config";     val title = "Mosaic setup" }
  case object BuildId             extends ScreenId { val name = "build";             val title = "Building steps" }
  case object PrintInstructionsId extends ScreenId { val name = "print-instructions"; val title = "Print";              override val overviewDescription = Some("Create a printable PDF guide") }

  /** Screen IDs shown on the overview page as link cards, in order. */
  val overviewScreenIds: List[ScreenId] =
    List(ImagesId, PalettesId, GridConfigsId, BuildConfigsId, BuildsId, PrintInstructionsId)

  /** Next screen in the overview flow (for the "Next →" button). Overview → first in list; last → Overview; editors → next after their gallery. */
  def nextInOverviewOrder(current: ScreenId): ScreenId =
    if (current == OverviewId) overviewScreenIds.head
    else {
      val idx = overviewScreenIds.indexOf(current)
      if (idx >= 0) {
        if (idx + 1 < overviewScreenIds.length) overviewScreenIds(idx + 1)
        else OverviewId
      } else
        current match {
          case GridConfigId   => PalettesId
          case PaletteId      => GridConfigsId
          case ImageUploadId  => BuildConfigsId
          case BuildConfigId  => BuildsId
          case BuildId        => PrintInstructionsId
          case _              => OverviewId
        }
    }
}

/** Data passed from one screen to the next when navigating. See docs/FLOW.md for the six-step flow. */
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

  /** Open grid config editor to edit an existing stored config. */
  final case class EditGridConfig(stored: StoredGridConfig) extends ScreenOutput

  /** Open palette editor to edit an existing stored palette. */
  final case class EditPalette(stored: StoredPalette) extends ScreenOutput

  /** Open palette editor with colors from an image (e.g. from Images gallery) so the user can save as a palette. */
  final case class NewPaletteFromImage(name: String, colors: Vector[Color]) extends ScreenOutput

  /** Open build config editor to edit an existing stored config. */
  final case class EditBuildConfig(stored: StoredBuildConfig) extends ScreenOutput

  /** Start the step-by-step build with the selected build config (plates → 16×16 cells). */
  final case class StartBuild(stored: StoredBuildConfig) extends ScreenOutput

  /** Resume a build from the builds list (has buildConfigRef + savedStepIndex). */
  final case class ResumeBuild(stored: StoredBuild) extends ScreenOutput
}

/** Stateless build definition: Palette + GridConfig + Image + Offset. */
final case class BuildConfig(
    grid: GridConfig,
    imageRef: String,
    paletteRef: String,
    offsetX: Int,
    offsetY: Int
)

object BuildConfig {
  given Encoder[BuildConfig] = deriveEncoder
  given Decoder[BuildConfig] = deriveDecoder
}

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
