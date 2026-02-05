package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId}
import tyrian.Html.*
import tyrian.*

/** Home page: links to Upload Images, Define Grid, Create Palettes, Mosaic Configurator, Mosaic Builder, Print Instructions. */
object OverviewScreen extends Screen {
  type Model = Unit
  type Msg   = OverviewMsg | NavigateNext

  val screenId: ScreenId = ScreenId.OverviewId

  private val linkCardStyle: String =
    "display: block; padding: 1rem 1.25rem; margin-bottom: 0.75rem; border: 1px solid #ccc; border-radius: 8px; background: #fff; text-decoration: none; color: #333; font-size: 1rem; cursor: pointer; transition: background 0.15s; text-align: left;"

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) =
    ((), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case OverviewMsg.GoTo(screenId) =>
      (model, Cmd.Emit(NavigateNext(screenId, None)))
    case OverviewMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(
      style := "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    )(
      h1(style := "margin-top: 0; margin-bottom: 1rem;")(text("Pixel Mosaic Maker")),
      p(style := "color: #555; margin-bottom: 1.5rem;")(
        text("Choose a gallery to manage saved items, or create new ones.")
      ),
      div(style := "display: flex; flex-direction: column;")(
        linkCard("Upload Images", ScreenId.ImagesId, "Upload and manage pixel images"),
        linkCard("Define Grid", ScreenId.GridConfigsId, "Grid configs (plate layouts)"),
        linkCard("Create Palettes", ScreenId.PalettesId, "Color palettes"),
        linkCard("Mosaic Configurator", ScreenId.BuildConfigsId, "Build configurations (grid + image + palette)"),
        linkCard("Mosaic Builder", ScreenId.BuildsId, "Step-by-step build runs"),
        linkCard("Print Instructions", ScreenId.PrintInstructionsId, "Generate PDF print instructions")
      )
    )

  private def linkCard(title: String, target: ScreenId, desc: String): Html[Msg] =
    button(
      style := linkCardStyle,
      onClick(OverviewMsg.GoTo(target))
    )(
      span(style := "font-weight: 600;")(text(title)),
      span(style := "display: block; font-size: 0.875rem; color: #666; margin-top: 0.25rem;")(
        text(desc)
      )
    )
}

enum OverviewMsg:
  case GoTo(screenId: ScreenId)
  case NoOp
