package mosaic.ui

import tyrian.*
import mosaic.domain.*

final case class Model()

enum Msg {
  case NoOp
}

object App extends TyrianApp[Msg, Model] {

  def init(flags: Map[String, String]): (Model, Cmd[Msg]) =
    (Model(), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[Msg]) = {
    case Msg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    Html.div(
      Html.h1("Mosaic"),
      Html.p("Scala.js + Tyrian is running 🚀")
    )

  def subscriptions(model: Model): Sub[Msg] =
    Sub.None
}
