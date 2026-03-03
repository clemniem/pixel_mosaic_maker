package clemniem

import cats.effect.IO
import clemniem.screens.{
  AboutScreen,
  BuildConfigGalleryScreen,
  BuildConfigScreen,
  BuildScreen,
  BuildsGalleryScreen,
  ImageUploadScreen,
  ImagesGalleryScreen,
  LayoutGalleryScreen,
  LayoutScreen,
  OverviewMsg,
  OverviewScreen,
  PaletteScreen,
  PalettesGalleryScreen,
  PrintGalleryScreen,
  PrintInstructionsScreen
}
import tyrian.*

import scala.scalajs.js.annotation.*

/** Root model: current screen wrapped in [[ActiveScreen]] (no raw `Any`). Registry is fixed at startup. */
final case class RootModel(
  registry: ScreenRegistry,
  currentScreenId: ScreenId,
  activeScreen: ActiveScreen)

@JSExportTopLevel("TyrianApp")
object PixelMosaicMaker extends TyrianIOApp[RootMsg, RootModel] {

  private val registry: ScreenRegistry =
    ScreenRegistry(
      screens = List(
        OverviewScreen,
        AboutScreen,
        LayoutGalleryScreen,
        LayoutScreen,
        PalettesGalleryScreen,
        PaletteScreen,
        ImagesGalleryScreen,
        ImageUploadScreen,
        BuildConfigGalleryScreen,
        BuildConfigScreen,
        BuildsGalleryScreen,
        BuildScreen,
        PrintGalleryScreen,
        PrintInstructionsScreen
      ),
      initialScreenId = ScreenId.OverviewId
    )

  def router: Location => RootMsg =
    Routing.none(RootMsg.HandleScreenMsg(registry.initialScreenId, OverviewMsg.NoOp))

  def init(flags: Map[String, String]): (RootModel, Cmd[IO, RootMsg]) = {
    val screen             = registry.screenFor(registry.initialScreenId).get
    val (active, rootCmd)  = ActiveScreen.fromInit(screen, None)
    (RootModel(registry, registry.initialScreenId, active), rootCmd)
  }

  def update(model: RootModel): RootMsg => (RootModel, Cmd[IO, RootMsg]) = {
    case RootMsg.NavigateTo(screenId, output) =>
      model.registry.screenFor(screenId) match {
        case None =>
          (model, Cmd.None)
        case Some(screen) =>
          val (active, cmd) = ActiveScreen.fromInit(screen, output)
          (model.copy(currentScreenId = screenId, activeScreen = active), cmd)
      }

    case RootMsg.HandleScreenMsg(screenId, msg) =>
      // NavigateNext is intercepted in Screen.wrapMsg → NavigateTo, so it never arrives here.
      if (screenId != model.currentScreenId) (model, Cmd.None)
      else {
        val (active, cmd) = model.activeScreen.update(msg)
        (model.copy(activeScreen = active), cmd)
      }
  }

  def view(model: RootModel): Html[RootMsg] =
    model.activeScreen.view

  def subscriptions(model: RootModel): Sub[IO, RootMsg] =
    model.activeScreen.subscriptions
}
