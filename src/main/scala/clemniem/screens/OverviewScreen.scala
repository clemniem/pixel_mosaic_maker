package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId}
import clemniem.common.nescss.NesCss
import org.scalajs.dom
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
    case OverviewMsg.ToggleTheme =>
      val run = IO(dom.document.body.classList.toggle("theme-gameboy"))
      (model, Cmd.Run(run)(_ => OverviewMsg.NoOp))
    case OverviewMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(
      `class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container screen-container--gameboy"
    )(
      h1(`class` := "screen-title")(
        text("Pixel M"),
        span(`class` := "title-theme-toggle", onClick(OverviewMsg.ToggleTheme))(text("o")),
        text("saic Maker")
      ),
      p(`class` := s"${NesCss.text} screen-intro")(
        text("Pick a step to manage your saved items or create new ones.")
      ),
      div(`class` := "flex-col flex-col--gap-1 screen-container-inner")(
        ScreenId.overviewScreenIds.flatMap { id =>
          id.overviewDescription.map(desc => linkCard(id.title, id, desc))
        }*
      ),
      div(`class` := "screen-about-link", onClick(OverviewMsg.GoTo(ScreenId.AboutId)))(text("about"))
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
  case ToggleTheme
  case NoOp
