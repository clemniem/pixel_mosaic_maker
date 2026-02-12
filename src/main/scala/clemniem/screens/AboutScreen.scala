package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId}
import clemniem.common.CmdUtils
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import tyrian.Html.*
import tyrian.*
import scala.scalajs.js

private val repoUrl = "https://github.com/clemniem/pixel_mosaic_maker"

/** About page: project description, tools, GitHub link, and refresh-app with confirmation. */
object AboutScreen extends Screen {
  type Model = Boolean
  type Msg   = AboutMsg | NavigateNext

  val screenId: ScreenId = ScreenId.AboutId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (false, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case AboutMsg.ShowRefreshConfirm =>
      (true, Cmd.None)
    case AboutMsg.ConfirmRefresh =>
      (model, CmdUtils.fireAndForget(refreshAppFromSW, AboutMsg.NoOp, _ => AboutMsg.NoOp))
    case AboutMsg.CancelRefresh =>
      (false, Cmd.None)
    case AboutMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case AboutMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def refreshAppFromSW: IO[Unit] = IO.delay {
    val f = js.Dynamic.global.selectDynamic("refreshApp")
    if (js.typeOf(f) == "function") {
      f.asInstanceOf[js.Function0[Unit]]()
    } else {
      dom.window.location.reload()
    }
  }

  def view(model: Model): Html[Msg] =
    div(
      `class` := s"${NesCss.screenContainer} screen-container--wide"
    )(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text("About")),
        GalleryLayout.backButton(AboutMsg.Back, "Overview")
      ),
      div(`class` := "about-content")(
        p(`class` := NesCss.text)(text("Pixel Mosaic Maker turns pixel art into printable mosaic instructions.")),
        p(`class` := NesCss.text)(text("Use Game Boy Camera photos or other low-res images.")),
        p(`class` := NesCss.text)(text("Get step-by-step guides to recreate the image with physical tiles or bricks (e.g. Lego plates, perler beads).")),
        p(`class` := NesCss.text)(text("You define a grid and upload images.")),
        p(`class` := NesCss.text)(text("You create palettes and build configs, then run a build.")),
        p(`class` := NesCss.text)(text("You get a PDF book: cover, overview, per-plate chapters with swatches, and layer-by-layer patch pages.")),
        p(`class` := NesCss.text)(text("Everything runs in the browser.")),
        p(`class` := NesCss.text)(text("Data is stored in LocalStorage. No server or account required.")),
        h2(`class` := "about-heading")(text("Libraries & tools")),
        div(`class` := "about-tools")(
          toolsTable
        ),
        h2(`class` := "about-heading")(text("Source")),
        p(`class` := NesCss.text)(
          a(
            href := repoUrl,
            Attribute("target", "_blank"),
            Attribute("rel", "noopener noreferrer"),
            `class` := "about-link about-source-icon-link"
          )(
            i(`class` := "nes-icon github is-large")()
          )
        ),
        h2(`class` := "about-heading")(text("Get latest version")),
        p(`class` := NesCss.text)(text("After a deploy you might still see an old version.")),
        p(`class` := NesCss.text)(text("The browser may be serving cached files.")),
        p(`class` := NesCss.text)(text("The button below unregisters the cache and reloads the page.")),
        (if (model)
          div(`class` := "about-refresh-confirm")(
            p(`class` := "about-refresh-confirm-text")(
              text("Unregister cache and reload now? The page will refresh.")
            ),
            div(`class` := "flex-row flex-row--tight")(
              button(`class` := NesCss.btnPrimary, onClick(AboutMsg.ConfirmRefresh))(text("Yes, refresh")),
              button(`class` := NesCss.btn, onClick(AboutMsg.CancelRefresh))(text("Cancel"))
            )
          )
        else
          button(`class` := NesCss.btn, onClick(AboutMsg.ShowRefreshConfirm))(text("Refresh app"))
      )
    )
  )

  private def toolsTable: Html[Msg] = {
    val rows = List(
      ("Tyrian", "Elm-style UI framework for Scala.js", "https://github.com/PurpleKingdomGames/tyrian"),
      ("NES.css", "Retro pixel-art CSS framework", "https://nostalgic-css.github.io/NES.css/"),
      ("jsPDF", "Client-side PDF generation", "https://github.com/parallax/jsPDF"),
      ("Scala.js", "Scala compiled to JavaScript", "https://www.scala-js.org/"),
      ("Parcel", "Dev server and bundler", "https://parceljs.org/")
    )
    div(`class` := "about-table-wrap")(
      table(`class` := "about-table")(
        thead(
          tr(
            th(`class` := NesCss.text)(text("Library")),
            th(`class` := NesCss.text)(text("Purpose"))
          )
        ),
        tbody(
          rows.map { case (name, purpose, url) =>
            tr(
              td(`class` := NesCss.text)(
                a(
                  href := url,
                  Attribute("target", "_blank"),
                  Attribute("rel", "noopener noreferrer"),
                  `class` := "about-link"
                )(text(name))
              ),
              td(`class` := NesCss.text)(text(purpose))
            )
          }*
        )
      )
    )
  }
}

enum AboutMsg:
  case ShowRefreshConfirm
  case ConfirmRefresh
  case CancelRefresh
  case Back
  case NoOp
