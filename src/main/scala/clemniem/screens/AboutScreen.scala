package clemniem.screens

import cats.effect.IO
import clemniem.{Screen, ScreenId}
import clemniem.common.CmdUtils
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import tyrian.Html.*
import tyrian.*
import scala.scalajs.js

private val repoUrl = "https://github.com/clemniem/pixel_mosaic_maker"

/** About page: project description, tools, GitHub link, refresh-app, and load sample data. */
object AboutScreen extends Screen {
  type Model = AboutState
  type Msg   = AboutMsg

  val screenId: ScreenId = ScreenId.AboutId

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) =
    (AboutState.Idle, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case AboutMsg.ShowRefreshConfirm =>
      (AboutState.ConfirmRefresh, Cmd.None)
    case AboutMsg.ConfirmRefresh =>
      (AboutState.Idle, CmdUtils.fireAndForget(refreshAppFromSW, AboutMsg.NoOp, _ => AboutMsg.NoOp))
    case AboutMsg.CancelRefresh =>
      (AboutState.Idle, Cmd.None)
    case AboutMsg.ShowSeedConfirm =>
      (AboutState.ConfirmSeed, Cmd.None)
    case AboutMsg.ConfirmSeed =>
      (AboutState.SeedLoading, CmdUtils.run(SeedData.seed(), AboutMsg.SeedDone.apply, _ => AboutMsg.SeedFailed))
    case AboutMsg.CancelSeed =>
      (AboutState.Idle, Cmd.None)
    case AboutMsg.SeedDone(count) =>
      (AboutState.SeedResult(Some(count)), Cmd.None)
    case AboutMsg.SeedFailed =>
      (AboutState.SeedResult(None), Cmd.None)
    case AboutMsg.DismissSeedResult =>
      (AboutState.Idle, Cmd.None)
    case AboutMsg.OpenUrl(url) =>
      (model, CmdUtils.fireAndForget(IO.delay { val _ = dom.window.open(url, "_blank") }, AboutMsg.NoOp, _ => AboutMsg.NoOp))
    case AboutMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case AboutMsg.NoOp =>
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
        p(`class` := NesCss.text)(text(
          "Get step-by-step guides to recreate the image with physical tiles or bricks (e.g. Lego sections, perler beads).")),
        p(`class` := NesCss.text)(text("You define a grid and upload images.")),
        p(`class` := NesCss.text)(text("You create palettes and build configs, then run a build.")),
        p(`class` := NesCss.text)(text(
          "You get a PDF book: cover, overview, per-section chapters with swatches, and layer-by-layer patch pages.")),
        p(`class` := NesCss.text)(text("Everything runs in the browser.")),
        p(`class` := NesCss.text)(text("Data is stored in LocalStorage. No server or account required.")),
        h2(`class` := "about-heading")(text("Sample data")),
        p(`class` := NesCss.text)(text("Load demo images, palettes, layouts, and builds to explore the full workflow.")),
        seedSection(model),
        h2(`class` := "about-heading")(text("Libraries & tools")),
        div(`class` := "about-tools")(
          toolsTable
        ),
        h2(`class` := "about-heading")(text("Source")),
        p(`class` := NesCss.text)(
          span(
            `class` := "about-link about-source-icon-link",
            onClick(AboutMsg.OpenUrl(repoUrl))
          )(
            i(`class` := "nes-icon github is-large")()
          )
        ),
        h2(`class` := "about-heading")(text("Get latest version")),
        p(`class` := NesCss.text)(text("After a deploy you might still see an old version.")),
        p(`class` := NesCss.text)(text("The browser may be serving cached files.")),
        p(`class` := NesCss.text)(text("The button below unregisters the cache and reloads the page.")),
        refreshSection(model)
      )
    )

  private def seedSection(model: Model): Html[Msg] =
    model match {
      case AboutState.ConfirmSeed =>
        div(`class` := "about-refresh-confirm")(
          p(`class` := "about-refresh-confirm-text")(
            text("Load sample data into all galleries? Existing items are kept.")
          ),
          div(`class` := "flex-row flex-row--tight")(
            button(`class` := NesCss.btnPrimary, onClick(AboutMsg.ConfirmSeed))(text("Yes, load")),
            button(`class` := NesCss.btn, onClick(AboutMsg.CancelSeed))(text("Cancel"))
          )
        )
      case AboutState.SeedLoading =>
        p(`class` := NesCss.text)(text("Loading\u2026"))
      case AboutState.SeedResult(Some(count)) =>
        div(`class` := "about-refresh-confirm")(
          p(`class` := "about-refresh-confirm-text")(
            text(if (count > 0) s"Added $count sample item(s). Reload to see them." else "Sample data already loaded.")
          ),
          div(`class` := "flex-row flex-row--tight")(
            (if (count > 0)
              button(`class` := NesCss.btnPrimary, onClick(AboutMsg.ConfirmRefresh))(text("Reload now"))
            else
              button(`class` := NesCss.btn, onClick(AboutMsg.DismissSeedResult))(text("OK")))
          )
        )
      case AboutState.SeedResult(None) =>
        div(`class` := "about-refresh-confirm")(
          p(`class` := "about-refresh-confirm-text")(text("Failed to load sample data.")),
          button(`class` := NesCss.btn, onClick(AboutMsg.DismissSeedResult))(text("OK"))
        )
      case _ =>
        button(`class` := NesCss.btn, onClick(AboutMsg.ShowSeedConfirm))(text("Load sample data"))
    }

  private def refreshSection(model: Model): Html[Msg] =
    model match {
      case AboutState.ConfirmRefresh =>
        div(`class` := "about-refresh-confirm")(
          p(`class` := "about-refresh-confirm-text")(
            text("Unregister cache and reload now? The page will refresh.")
          ),
          div(`class` := "flex-row flex-row--tight")(
            button(`class` := NesCss.btnPrimary, onClick(AboutMsg.ConfirmRefresh))(text("Yes, refresh")),
            button(`class` := NesCss.btn, onClick(AboutMsg.CancelRefresh))(text("Cancel"))
          )
        )
      case _ =>
        button(`class` := NesCss.btn, onClick(AboutMsg.ShowRefreshConfirm))(text("Refresh app"))
    }

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
                span(
                  `class` := "about-link",
                  onClick(AboutMsg.OpenUrl(url))
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

enum AboutState {
  case Idle
  case ConfirmRefresh
  case ConfirmSeed
  case SeedLoading
  case SeedResult(count: Option[Int])
}

enum AboutMsg {
  case ShowRefreshConfirm
  case ConfirmRefresh
  case CancelRefresh
  case ShowSeedConfirm
  case ConfirmSeed
  case CancelSeed
  case SeedDone(count: Int)
  case SeedFailed
  case DismissSeedResult
  case OpenUrl(url: String)
  case Back
  case NoOp
}
