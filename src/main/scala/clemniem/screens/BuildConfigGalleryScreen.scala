package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, StorageKeys, StoredBuildConfig}
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

/** Gallery of saved build configs. Empty state: "+ Create BuildConfig". */
object BuildConfigGalleryScreen extends Screen {
  type Model = Option[List[StoredBuildConfig]]
  type Msg   = BuildConfigGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildConfigsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      BuildConfigGalleryMsg.Loaded.apply,
      _ => BuildConfigGalleryMsg.Loaded(Nil),
      (_, _) => BuildConfigGalleryMsg.Loaded(Nil)
    )
    (None, cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildConfigGalleryMsg.Loaded(list) =>
      (Some(list), Cmd.None)
    case BuildConfigGalleryMsg.CreateNew =>
      (model, Cmd.None)
    case BuildConfigGalleryMsg.Back =>
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
            h1(style := "margin: 0;")(text("Build configs")),
            button(
              style := "padding: 6px 12px; cursor: pointer;",
              onClick(BuildConfigGalleryMsg.Back)
            )(text("← Overview"))
          ),
          if (list.isEmpty)
            emptyState("Create BuildConfig", BuildConfigGalleryMsg.CreateNew)
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map(item =>
                div(
                  style := "padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
                )(
                  span(style := "font-weight: 500;")(text(item.name)),
                  span(style := "color: #666; font-size: 0.875rem; margin-left: 0.5rem;")(
                    text(s"${item.config.grid.width}×${item.config.grid.height} · ${item.config.imageRef}")
                  )
                )
              ) :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(BuildConfigGalleryMsg.CreateNew)
              )(text("+ Create BuildConfig")))*
            )
        )
    }
  }

  private def emptyState(createLabel: String, createMsg: Msg): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text("No build configs yet.")),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(s"+ $createLabel"))
    )
}

enum BuildConfigGalleryMsg:
  case Loaded(list: List[StoredBuildConfig])
  case CreateNew
  case Back
