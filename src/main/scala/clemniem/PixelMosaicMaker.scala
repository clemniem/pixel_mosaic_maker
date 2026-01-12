package clemniem

import cats.effect.IO
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object PixelMosaicMaker extends TyrianIOApp[Msg, Model] {


  def router: Location => Msg =
    Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (
      Model(
        grid = GridConfig.make(List(48, 16, 48), List(48, 32, 48))
      ),
      Cmd.Emit(Msg.DrawGrid)
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.DrawGrid =>
      (
        model,
        Cmd.SideEffect(drawGrid(model.grid))
      )
    case Msg.NoOp => (model, Cmd.None)


  def view(model: Model): Html[Msg] = {
    div(
      h3("Grid preview"),
      button(
        onClick(Msg.DrawGrid)
      )(
        text("Redraw grid")
      ),
      div(
        onLoad(Msg.DrawGrid))(
        canvas(
          id := "grid-canvas",
          width := model.grid.width,
          height := model.grid.height,
          style := "border: 1px solid black;"
        )()
      )
    )
  }

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

  def drawGrid(grid: GridConfig): IO[Unit] = {
    IO {
      import org.scalajs.dom

      val canvas =
        dom.document
          .getElementById("grid-canvas")
          .asInstanceOf[dom.html.Canvas]

      val ctx =
        canvas.getContext("2d")
          .asInstanceOf[dom.CanvasRenderingContext2D]

      ctx.clearRect(0, 0, grid.width, grid.height)
      ctx.strokeStyle = "#000000"
      grid.parts.foreach { part =>
        ctx.strokeRect(
          part.x,
          part.y,
          part.width,
          part.height
        )
      }
    }
  }
}

final case class Model(grid: GridConfig)

enum Msg:
  case DrawGrid
  case NoOp
