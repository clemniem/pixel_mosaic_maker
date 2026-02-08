package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId}
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Home page: links to Upload Images, Define Grid, Create Palettes, Mosaic Configurator, Mosaic Builder, Print Instructions. */
object OverviewScreen extends Screen {
  type Model = Unit
  type Msg   = OverviewMsg | NavigateNext

  val screenId: ScreenId = ScreenId.OverviewId

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
      `class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container"
    )(
      h1(`class` := "screen-title")(text("Pixel Mosaic Maker")),
      p(`class` := s"${NesCss.text} screen-intro")(
        text("Choose a gallery to manage saved items, or create new ones.")
      ),
      div(`class` := "flex-col flex-col--gap-1")(
        ScreenId.overviewScreenIds.flatMap { id =>
          id.overviewDescription.map(desc => linkCard(id.title, id, desc))
        }*
      )
    )

  private def linkCard(title: String, target: ScreenId, desc: String): Html[Msg] =
    button(
      `class` := s"${NesCss.btn} link-card",
      onClick(OverviewMsg.GoTo(target))
    )(
      span(`class` := "link-card-title")(text(title)),
      span(`class` := "link-card-desc")(text(desc))
    )
}

enum OverviewMsg:
  case GoTo(screenId: ScreenId)
  case NoOp
