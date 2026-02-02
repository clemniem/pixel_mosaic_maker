package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, StorageKeys, StoredBuild}
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

/** Gallery of saved builds. Empty state: "+ Create Build". */
object BuildsGalleryScreen extends Screen {
  type Model = Option[List[StoredBuild]]
  type Msg   = BuildsGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.builds)(
      BuildsGalleryMsg.Loaded.apply,
      _ => BuildsGalleryMsg.Loaded(Nil),
      (_, _) => BuildsGalleryMsg.Loaded(Nil)
    )
    (None, cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildsGalleryMsg.Loaded(list) =>
      (Some(list), Cmd.None)
    case BuildsGalleryMsg.CreateNew =>
      (model, Cmd.None)
    case BuildsGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    model match {
      case None =>
        div(style := container)(p(text("Loading…")))
      case Some(list) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Builds")),
            button(style := "padding: 6px 12px; cursor: pointer;", onClick(BuildsGalleryMsg.Back))(
              text("← Overview")
            )
          ),
          if (list.isEmpty)
            emptyState("Create Build", BuildsGalleryMsg.CreateNew)
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map(item =>
                div(
                  style := "padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
                )(span(style := "font-weight: 500;")(text(item.name)))
              ) :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(BuildsGalleryMsg.CreateNew)
              )(text("+ Create Build")))*
            )
        )
    }
  }

  private def emptyState(createLabel: String, createMsg: Msg): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text("No builds yet.")),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(s"+ $createLabel"))
    )
}

enum BuildsGalleryMsg:
  case Loaded(list: List[StoredBuild])
  case CreateNew
  case Back
