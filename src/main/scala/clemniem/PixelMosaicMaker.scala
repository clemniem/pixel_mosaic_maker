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
  PrintInstructionsScreen
}
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

/** Root model: current screen and its model (type-erased). Registry is fixed at startup. */
final case class RootModel(
  registry: ScreenRegistry,
  currentScreenId: ScreenId,
  currentModel: Any)

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
        PrintInstructionsScreen
      ),
      initialScreenId = ScreenId.OverviewId
    )

  def router: Location => RootMsg =
    Routing.none(RootMsg.HandleScreenMsg(registry.initialScreenId, OverviewMsg.NoOp))

  def init(flags: Map[String, String]): (RootModel, Cmd[IO, RootMsg]) = {
    val screen       = registry.screenFor(registry.initialScreenId).get
    val (model, cmd) = screen.init(None)
    (
      RootModel(registry, registry.initialScreenId, model),
      cmd.map(screen.wrapMsg)
    )
  }

  def update(model: RootModel): RootMsg => (RootModel, Cmd[IO, RootMsg]) = {
    case RootMsg.NavigateTo(screenId, output) =>
      model.registry.screenFor(screenId) match {
        case None =>
          (model, Cmd.None)
        case Some(screen) =>
          val (newScreenModel, cmd) = screen.init(output)
          (
            model.copy(currentScreenId = screenId, currentModel = newScreenModel),
            cmd.map(screen.wrapMsg)
          )
      }

    case RootMsg.HandleScreenMsg(screenId, msg) =>
      msg match {
        case n: NavigateNext =>
          model.registry.screenFor(n.screenId) match {
            case None => (model, Cmd.None)
            case Some(screen) =>
              val (newScreenModel, cmd) = screen.init(n.output)
              (
                model.copy(currentScreenId = n.screenId, currentModel = newScreenModel),
                cmd.map(screen.wrapMsg)
              )
          }
        case _ =>
          if (screenId != model.currentScreenId) (model, Cmd.None)
          else
            model.registry.screenFor(screenId) match {
              case None =>
                (model, Cmd.None)
              case Some(screen) =>
                val (newScreenModel, cmd) =
                  screen.update(model.currentModel.asInstanceOf[screen.Model])(
                    msg.asInstanceOf[screen.Msg]
                  )
                (
                  model.copy(currentModel = newScreenModel),
                  cmd.map(screen.wrapMsg)
                )
            }
      }
  }

  def view(model: RootModel): Html[RootMsg] =
    model.registry.screenFor(model.currentScreenId) match {
      case None =>
        div(text("Unknown screen"))
      case Some(screen) =>
        screen
          .view(model.currentModel.asInstanceOf[screen.Model])
          .map(screen.wrapMsg)
    }

  def subscriptions(model: RootModel): Sub[IO, RootMsg] =
    model.registry.screenFor(model.currentScreenId) match {
      case None => Sub.None
      case Some(screen) =>
        screen
          .subscriptions(model.currentModel.asInstanceOf[screen.Model])
          .map(screen.wrapMsg)
    }
}
