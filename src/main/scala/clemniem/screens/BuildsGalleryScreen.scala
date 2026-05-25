package clemniem.screens

import cats.effect.IO
import clemniem.{
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuild,
  StoredBuildConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, CmdUtils, Loadable, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Builds gallery: list of builds (resume any). Start new build shows dropdown to pick build config. */
object BuildsGalleryScreen extends Screen {
  type Model = BuildsGalleryModel
  type Msg   = BuildsGalleryMsg

  val screenId: ScreenId = ScreenId.BuildsId

  private val buildPreviewWidth  = CanvasUtils.galleryPreviewWidth
  private val buildPreviewHeight = CanvasUtils.galleryPreviewHeight
  private val patchSize          = 16

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val model      = BuildsGalleryModel(Gallery.initState, None, None, None, None, showNewBuildDropdown = false)
    val loadBuilds = Gallery.loadCmd(
      StorageKeys.builds,
      BuildsGalleryMsg.LoadedBuilds.apply,
      (msg, _) => BuildsGalleryMsg.LoadFailed(msg))
    val loadConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      BuildsGalleryMsg.LoadedBuildConfigs.apply,
      (_, _) => BuildsGalleryMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildsGalleryMsg.LoadedImages.apply,
      (_, _) => BuildsGalleryMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildsGalleryMsg.LoadedPalettes.apply,
      (_, _) => BuildsGalleryMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuilds, loadConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildsGalleryMsg.LoadedBuilds(list) =>
      val valid = list.filter(_.buildConfigRef.nonEmpty)
      val next  = model.copy(gallery = Gallery.onLoaded(model.gallery, valid, GalleryLayout.defaultPageSize))
      val cmd   = if (next.canDrawPreviews) drawAllPreviewsCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedBuildConfigs(list) =>
      val next = model.copy(buildConfigs = Some(list))
      val cmd  = if (next.canDrawPreviews) drawAllPreviewsCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      val cmd  = if (next.canDrawPreviews) drawAllPreviewsCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      val cmd  = if (next.canDrawPreviews) drawAllPreviewsCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.DrawBuildPreview(item) =>
      (
        model,
        CmdUtils.fireAndForget(
          drawBuildPreview(
            item,
            model.buildConfigs.getOrElse(Nil),
            model.images.getOrElse(Nil),
            model.palettes.getOrElse(Nil)),
          BuildsGalleryMsg.NoOp,
          _ => BuildsGalleryMsg.NoOp
        ))
    case BuildsGalleryMsg.ShowNewBuildDropdown =>
      (model.copy(showNewBuildDropdown = true), Cmd.None)
    case BuildsGalleryMsg.SetSelectedBuildConfigId(id) =>
      (model.copy(selectedBuildConfigId = Some(id)), Cmd.None)
    case BuildsGalleryMsg.StartNewBuild =>
      (for {
        configs <- model.buildConfigs
        id      <- model.selectedBuildConfigId.orElse(configs.headOption.map(_.id))
        stored  <- configs.find(_.id == id)
      } yield stored)
        .fold[(Model, Cmd[IO, Msg])]((model, Cmd.None))(stored =>
          (
            model.copy(showNewBuildDropdown = false, selectedBuildConfigId = None),
            navCmd(ScreenId.BuildId, Some(ScreenOutput.StartBuild(stored)))))
    case BuildsGalleryMsg.CancelNewBuild =>
      (model.copy(showNewBuildDropdown = false, selectedBuildConfigId = None), Cmd.None)
    case BuildsGalleryMsg.ResumeBuild(stored) =>
      (model, navCmd(ScreenId.BuildId, Some(ScreenOutput.ResumeBuild(stored))))
    case BuildsGalleryMsg.Delete(stored) =>
      (model.copy(gallery = Gallery.onRequestDelete(model.gallery, stored.id)), Cmd.None)
    case BuildsGalleryMsg.ConfirmDelete(id) =>
      val (gs, cmd) = Gallery.onConfirmDelete(
        model.gallery,
        id,
        StorageKeys.builds,
        GalleryLayout.defaultPageSize,
        BuildsGalleryMsg.CancelDelete)
      (model.copy(gallery = gs), cmd)
    case BuildsGalleryMsg.CancelDelete =>
      (model.copy(gallery = Gallery.onCancelDelete(model.gallery)), Cmd.None)
    case BuildsGalleryMsg.PreviousPage =>
      val next = model.copy(gallery = Gallery.onPreviousPage(model.gallery))
      val cmd  = if (next.canDrawPreviews) redrawCurrentPageCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.NextPage =>
      val next = model.copy(gallery = Gallery.onNextPage(model.gallery, GalleryLayout.defaultPageSize))
      val cmd  = if (next.canDrawPreviews) redrawCurrentPageCmd(next) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case BuildsGalleryMsg.LoadFailed(error) =>
      (model.copy(gallery = Gallery.onLoadFailed(model.gallery, error)), Cmd.None)
    case BuildsGalleryMsg.ClearData =>
      val cmd = LocalStorageUtils.remove(StorageKeys.builds)(
        _ => BuildsGalleryMsg.Retry,
        (_, _) => BuildsGalleryMsg.Retry
      )
      (model.copy(gallery = Gallery.initState), cmd)
    case BuildsGalleryMsg.Retry =>
      init(None)
    case BuildsGalleryMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val backBtn = GalleryLayout.backButton(BuildsGalleryMsg.Back, "Overview")
    val nextBtn = GalleryLayout.nextButton(navMsg(ScreenFlow.nextInOverviewOrder(screenId), None))
    (model.gallery.items, model.buildConfigs) match {
      case (Loadable.Loading, _) | (_, None) =>
        GalleryLayout(
          screenId.title,
          backBtn,
          p(`class` := NesCss.text)(text("Loading\u2026")),
          shortHeader = false,
          Some(nextBtn))
      case (Loadable.Failed(error), _) =>
        GalleryLayout(
          screenId.title,
          backBtn,
          Gallery.failedView(error, BuildsGalleryMsg.ClearData, BuildsGalleryMsg.Retry),
          shortHeader = false,
          Some(nextBtn))
      case (Loadable.Loaded(builds), Some(configs)) =>
        val bottomSection =
          if (model.showNewBuildDropdown)
            div(`class` := s"${NesCss.container} ${NesCss.containerRounded} dropdown-panel")(
              div(`class` := "dropdown-panel-title")(text("New build from setup:")),
              div(`class` := "flex-row flex-row--tight")(
                select(
                  `class` := NesCss.input,
                  style   := "min-width: 14rem;",
                  value   := model.selectedBuildConfigId.orElse(configs.headOption.map(_.id)).getOrElse(""),
                  onInput(id =>
                    BuildsGalleryMsg.SetSelectedBuildConfigId(
                      if (id.isEmpty) configs.headOption.map(_.id).getOrElse("") else id))
                )(
                  configs.map { c =>
                    option(value := c.id)(text(c.name))
                  }*
                ),
                button(`class` := NesCss.btnPrimary, onClick(BuildsGalleryMsg.StartNewBuild))(text("Start")),
                button(`class` := NesCss.btn, onClick(BuildsGalleryMsg.CancelNewBuild))(text("Cancel"))
              )
            )
          else
            button(`class` := NesCss.btnPrimary, onClick(BuildsGalleryMsg.ShowNewBuildDropdown))(
              text("+ Start new build"))
        val content =
          if (builds.isEmpty)
            if (model.showNewBuildDropdown)
              div(`class` := GalleryLayout.galleryListClass)(bottomSection)
            else
              GalleryEmptyState("No builds yet.", "+ Start new build", BuildsGalleryMsg.ShowNewBuildDropdown)
          else
            GalleryLayout.paginatedListWith(
              builds,
              model.gallery.currentPage,
              GalleryLayout.defaultPageSize,
              bottomSection,
              b => entryCard(b, configs, model.gallery.pendingDeleteId.contains(b.id)),
              BuildsGalleryMsg.PreviousPage,
              BuildsGalleryMsg.NextPage
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = false, Some(nextBtn))
    }
  }

  private def entryCard(item: StoredBuild, configs: List[StoredBuildConfig], confirmingDelete: Boolean): Html[Msg] = {
    val configOpt   = configs.find(_.id == item.buildConfigRef)
    val configName  = configOpt.map(_.name).getOrElse(item.buildConfigRef)
    val totalSteps  = configOpt.map(c => BuildScreen.stepsForConfig(c.config).size).getOrElse(0)
    val currentStep = item.savedStepIndex.getOrElse(0).max(0).min(if (totalSteps <= 0) 0 else totalSteps - 1)
    val stepLabel   = if (totalSteps <= 0) "Step 0 of 0" else s"Step ${currentStep + 1} of $totalSteps"
    val progressPct = if (totalSteps <= 0) 0.0 else ((currentStep + 1).toDouble / totalSteps * 100).min(100.0)

    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card gallery-card--start")(
      div(`class` := "gallery-preview-wrap")(
        div(onLoad(BuildsGalleryMsg.DrawBuildPreview(item)))(
          canvas(
            id      := s"builds-preview-${item.id}",
            width   := buildPreviewWidth,
            height  := buildPreviewHeight,
            `class` := "gallery-preview-canvas"
          )()
        )
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(configName)),
        div(style := "margin-top: 0.5rem;")(
          span(`class` := "nes-text")(text(stepLabel)),
          div(`class` := "progress-bar")(
            div(`class` := "progress-bar-fill", style := s"width: ${progressPct}%; background: #2e7d32;")()
          )
        ),
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          BuildsGalleryMsg.ConfirmDelete.apply,
          BuildsGalleryMsg.CancelDelete,
          button(`class` := NesCss.btnPrimary, onClick(BuildsGalleryMsg.ResumeBuild(item)))(text("Resume")),
          button(`class` := NesCss.btnError, onClick(BuildsGalleryMsg.Delete(item)))(text("Delete"))
        )
      )
    )
  }

  private def drawAllPreviewsCmd(model: BuildsGalleryModel): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(drawAllBuildPreviews(model), BuildsGalleryMsg.NoOp, _ => BuildsGalleryMsg.NoOp)

  private def drawAllBuildPreviews(model: BuildsGalleryModel): IO[Unit] =
    model.gallery.items
      .getOrElse(Nil: List[StoredBuild])
      .foldLeft(IO.unit)((acc, build) =>
        acc.flatMap(_ =>
          drawBuildPreview(
            build,
            model.buildConfigs.getOrElse(Nil),
            model.images.getOrElse(Nil),
            model.palettes.getOrElse(Nil))))

  private def redrawCurrentPageCmd(model: BuildsGalleryModel): Cmd[IO, Msg] = {
    val builds = model.gallery.items.getOrElse(Nil: List[StoredBuild])
    if (builds.isEmpty) Cmd.None
    else {
      val start    = (model.gallery.currentPage - 1) * GalleryLayout.defaultPageSize
      val slice    = builds.slice(start, start + GalleryLayout.defaultPageSize)
      val configs  = model.buildConfigs.getOrElse(Nil)
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      CmdUtils.fireAndForget(
        CanvasUtils.runAfterFrames(3)(slice.foldLeft(IO.unit)((acc, build) =>
          acc.flatMap(_ => drawBuildPreview(build, configs, images, palettes)))),
        BuildsGalleryMsg.NoOp,
        _ => BuildsGalleryMsg.NoOp
      )
    }
  }

  private def drawBuildPreview(
    build: StoredBuild,
    configs: List[StoredBuildConfig],
    images: List[StoredImage],
    palettes: List[StoredPalette]
  ): IO[Unit] = {
    val configOpt = configs.find(_.id == build.buildConfigRef)
    configOpt match {
      case None =>
        CanvasUtils.drawGalleryPreview(s"builds-preview-${build.id}") { (_: Canvas, ctx: CanvasRenderingContext2D) =>
          CanvasUtils.drawCenteredErrorText(ctx, buildPreviewWidth, buildPreviewHeight, "Missing config")
        }
      case Some(stored) =>
        val steps       = BuildScreen.stepsForConfig(stored.config)
        val stepIndex   = build.savedStepIndex.getOrElse(0).max(0).min(if (steps.isEmpty) 0 else steps.length - 1)
        val currentStep = if (steps.isEmpty) None else Some(steps(stepIndex))
        val picOpt      = clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes)

        CanvasUtils.drawGalleryPreview(s"builds-preview-${build.id}")((_: Canvas, ctx: CanvasRenderingContext2D) =>
          renderers.BuildPreviewRenderer.drawGalleryPreview(
            ctx,
            picOpt,
            stored.config.grid,
            stored.config.offsetX,
            stored.config.offsetY,
            currentStep,
            patchSize,
            buildPreviewWidth,
            buildPreviewHeight))
    }
  }
}

final case class BuildsGalleryModel(
  gallery: Gallery.State[StoredBuild],
  buildConfigs: Option[List[StoredBuildConfig]],
  images: Option[List[StoredImage]],
  palettes: Option[List[StoredPalette]],
  selectedBuildConfigId: Option[String],
  showNewBuildDropdown: Boolean) {
  def canDrawPreviews: Boolean =
    gallery.items.isLoaded && buildConfigs.isDefined && images.isDefined && palettes.isDefined
}

enum BuildsGalleryMsg {
  case LoadedBuilds(list: List[StoredBuild])
  case LoadFailed(error: String)
  case ClearData
  case Retry
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case DrawBuildPreview(build: StoredBuild)
  case ShowNewBuildDropdown
  case SetSelectedBuildConfigId(id: String)
  case StartNewBuild
  case CancelNewBuild
  case ResumeBuild(stored: StoredBuild)
  case Delete(stored: StoredBuild)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
  case NoOp
}
