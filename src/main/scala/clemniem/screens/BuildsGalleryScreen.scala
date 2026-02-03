package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredBuild, StoredBuildConfig}
import clemniem.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

/** Builds gallery: list of builds (resume any). Start new build shows dropdown to pick build config. */
object BuildsGalleryScreen extends Screen {
  type Model = BuildsGalleryModel
  type Msg   = BuildsGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = BuildsGalleryModel(None, None, None, showNewBuildDropdown = false)
    val loadBuilds = LocalStorageUtils.loadList(StorageKeys.builds)(
      BuildsGalleryMsg.LoadedBuilds.apply,
      _ => BuildsGalleryMsg.LoadedBuilds(Nil),
      (_, _) => BuildsGalleryMsg.LoadedBuilds(Nil)
    )
    val loadConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      BuildsGalleryMsg.LoadedBuildConfigs.apply,
      _ => BuildsGalleryMsg.LoadedBuildConfigs(Nil),
      (_, _) => BuildsGalleryMsg.LoadedBuildConfigs(Nil)
    )
    (model, Cmd.Batch(loadBuilds, loadConfigs))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildsGalleryMsg.LoadedBuilds(list) =>
      val valid = list.filter(_.buildConfigRef.nonEmpty)
      (model.copy(builds = Some(valid)), Cmd.None)
    case BuildsGalleryMsg.LoadedBuildConfigs(list) =>
      (model.copy(buildConfigs = Some(list)), Cmd.None)
    case BuildsGalleryMsg.ShowNewBuildDropdown =>
      (model.copy(showNewBuildDropdown = true), Cmd.None)
    case BuildsGalleryMsg.SetSelectedBuildConfigId(id) =>
      (model.copy(selectedBuildConfigId = Some(id)), Cmd.None)
    case BuildsGalleryMsg.StartNewBuild =>
      (for {
        configs <- model.buildConfigs
        id      <- model.selectedBuildConfigId.orElse(configs.headOption.map(_.id))
        stored  <- configs.find(_.id == id)
      } yield NavigateNext(ScreenId.BuildId, Some(ScreenOutput.StartBuild(stored))))
        .fold[(Model, Cmd[IO, Msg])]((model, Cmd.None))(nav =>
          (model.copy(showNewBuildDropdown = false, selectedBuildConfigId = None), Cmd.Emit(nav))
        )
    case BuildsGalleryMsg.CancelNewBuild =>
      (model.copy(showNewBuildDropdown = false, selectedBuildConfigId = None), Cmd.None)
    case BuildsGalleryMsg.ResumeBuild(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildId, Some(ScreenOutput.ResumeBuild(stored)))))
    case BuildsGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    (model.builds, model.buildConfigs) match {
      case (None, _) | (_, None) =>
        div(style := container)(p(text("Loading…")))
      case (Some(builds), Some(configs)) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Builds")),
            button(style := "padding: 6px 12px; cursor: pointer;", onClick(BuildsGalleryMsg.Back))(
              text("← Overview")
            )
          ),
          div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
            builds.map(b => entryCard(b, configs))*
          ),
          if (model.showNewBuildDropdown)
            div(
              style := "margin-top: 1rem; padding: 1rem; border: 1px solid #ccc; border-radius: 8px; background: #fafafa;"
            )(
              div(style := "font-weight: 500; margin-bottom: 0.5rem;")(text("New build from config:")),
              div(style := "display: flex; align-items: center; gap: 0.5rem; flex-wrap: wrap;")(
                select(
                  style := "padding: 6px 10px; min-width: 14rem;",
                  value := model.selectedBuildConfigId.orElse(configs.headOption.map(_.id)).getOrElse(""),
                  onInput(id => BuildsGalleryMsg.SetSelectedBuildConfigId(if (id.isEmpty) configs.headOption.map(_.id).getOrElse("") else id))
                )(
                  configs.map { c =>
                    option(value := c.id)(text(c.name))
                  }*
                ),
                button(
                  style := "padding: 6px 14px; cursor: pointer; background: #1565c0; color: #fff; border: none; border-radius: 4px; font-weight: 500;",
                  onClick(BuildsGalleryMsg.StartNewBuild)
                )(text("Start")),
                button(
                  style := "padding: 6px 12px; cursor: pointer; border: 1px solid #999; border-radius: 4px; background: #fff;",
                  onClick(BuildsGalleryMsg.CancelNewBuild)
                )(text("Cancel"))
              )
            )
          else
            button(
              style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
              onClick(BuildsGalleryMsg.ShowNewBuildDropdown)
            )(text("+ Start new build"))
        )
    }
  }

  private def entryCard(item: StoredBuild, configs: List[StoredBuildConfig]): Html[Msg] = {
    val configName = configs.find(_.id == item.buildConfigRef).map(_.name).getOrElse(item.buildConfigRef)
    val stepText   = item.savedStepIndex.fold("")(s => s" · step ${s + 1}")
    div(
      style := "display: flex; align-items: center; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(s"$configName$stepText")
        )
      ),
      button(
        style := "padding: 4px 10px; cursor: pointer; flex-shrink: 0;",
        onClick(BuildsGalleryMsg.ResumeBuild(item))
      )(text("Resume"))
    )
  }
}

final case class BuildsGalleryModel(
    builds: Option[List[StoredBuild]],
    buildConfigs: Option[List[StoredBuildConfig]],
    selectedBuildConfigId: Option[String],
    showNewBuildDropdown: Boolean
)

enum BuildsGalleryMsg:
  case LoadedBuilds(list: List[StoredBuild])
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case ShowNewBuildDropdown
  case SetSelectedBuildConfigId(id: String)
  case StartNewBuild
  case CancelNewBuild
  case ResumeBuild(stored: StoredBuild)
  case Back
