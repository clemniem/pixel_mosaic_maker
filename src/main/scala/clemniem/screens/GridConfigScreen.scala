package clemniem.screens

import cats.effect.IO
import clemniem.{GridConfig, Screen, ScreenId}
import tyrian.Html.*
import tyrian.*

/** Step 1: Define grid of plates (Lego-style). Instructions are generated per plate; plate cells are 16×16 for now. */
object GridConfigScreen extends Screen {
  type Model = GridConfigModel
  type Msg   = GridConfigMsg

  val screenId: ScreenId = ScreenId.GridConfigId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (
      GridConfigModel(GridConfig.make(List(48, 16, 48), List(48, 32, 48))),
      Cmd.Emit(GridConfigMsg.DrawGrid)
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case GridConfigMsg.DrawGrid =>
      (model, Cmd.SideEffect(drawGrid(model.grid)))
    case GridConfigMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(
      h3("Grid config"),
      p("Define the grid of plates (Lego-style). Instructions will be generated per plate."),
      button(onClick(GridConfigMsg.DrawGrid))(text("Redraw grid")),
      div(onLoad(GridConfigMsg.DrawGrid))(
        canvas(
          id := "grid-canvas",
          width := model.grid.width,
          height := model.grid.height,
          style := "border: 1px solid black;"
        )()
      )
    )

  def drawGrid(grid: GridConfig): IO[Unit] =
    IO {
      import org.scalajs.dom
      val canvas = dom.document
        .getElementById("grid-canvas")
        .asInstanceOf[dom.html.Canvas]
      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      ctx.clearRect(0, 0, grid.width, grid.height)
      ctx.strokeStyle = "#000000"
      grid.parts.foreach { part =>
        ctx.strokeRect(part.x, part.y, part.width, part.height)
      }
    }
}

final case class GridConfigModel(grid: GridConfig)

enum GridConfigMsg:
  case DrawGrid
  case NoOp
