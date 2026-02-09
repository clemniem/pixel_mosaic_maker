package clemniem.screens

import cats.effect.IO
import clemniem.{
  NavigateNext,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuild,
  StoredBuildConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Builds gallery: list of builds (resume any). Start new build shows dropdown to pick build config. */
object BuildsGalleryScreen extends Screen {
  type Model = BuildsGalleryModel
  type Msg   = BuildsGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildsId

  private val buildPreviewWidth  = 160
  private val buildPreviewHeight = 100
  private val patchSize          = 16

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = BuildsGalleryModel(None, None, None, None, None, showNewBuildDropdown = false, pendingDeleteId = None, currentPage = 1)
    val loadBuilds   = LocalStorageUtils.loadList(StorageKeys.builds)(
      BuildsGalleryMsg.LoadedBuilds.apply,
      _ => BuildsGalleryMsg.LoadedBuilds(Nil),
      (_, _) => BuildsGalleryMsg.LoadedBuilds(Nil)
    )
    val loadConfigs  = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      BuildsGalleryMsg.LoadedBuildConfigs.apply,
      _ => BuildsGalleryMsg.LoadedBuildConfigs(Nil),
      (_, _) => BuildsGalleryMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages   = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildsGalleryMsg.LoadedImages.apply,
      _ => BuildsGalleryMsg.LoadedImages(Nil),
      (_, _) => BuildsGalleryMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildsGalleryMsg.LoadedPalettes.apply,
      _ => BuildsGalleryMsg.LoadedPalettes(Nil),
      (_, _) => BuildsGalleryMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuilds, loadConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildsGalleryMsg.LoadedBuilds(list) =>
      val valid  = list.filter(_.buildConfigRef.nonEmpty)
      val maxPage = if (valid.isEmpty) 1 else ((valid.size - 1) / GalleryLayout.defaultPageSize) + 1
      val next   = model.copy(builds = Some(valid), currentPage = model.currentPage.min(maxPage).max(1))
      val cmd    = if (next.canDrawPreviews) Cmd.SideEffect(drawAllBuildPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedBuildConfigs(list) =>
      val next = model.copy(buildConfigs = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllBuildPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllBuildPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllBuildPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.DrawBuildPreview(item) =>
      (model, Cmd.SideEffect(drawBuildPreview(item, model.buildConfigs.getOrElse(Nil), model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))))
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
    case BuildsGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case BuildsGalleryMsg.ConfirmDelete(id) =>
      model.builds match {
        case Some(list) =>
          val newList = list.filterNot(_.id == id)
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.builds, newList)(
            _ => BuildsGalleryMsg.CancelDelete,
            (_, _) => BuildsGalleryMsg.CancelDelete
          )
          val maxPage = if (newList.isEmpty) 1 else ((newList.size - 1) / GalleryLayout.defaultPageSize) + 1
          (model.copy(builds = Some(newList), pendingDeleteId = None, currentPage = model.currentPage.min(maxPage).max(1)), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case BuildsGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case BuildsGalleryMsg.PreviousPage =>
      val next = model.copy(currentPage = (model.currentPage - 1).max(1))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next))) else Cmd.None
      (next, cmd)
    case BuildsGalleryMsg.NextPage =>
      model.builds match {
        case Some(builds) =>
          val maxPage = if (builds.isEmpty) 1 else ((builds.size - 1) / GalleryLayout.defaultPageSize) + 1
          val next    = model.copy(currentPage = (model.currentPage + 1).min(maxPage))
          val cmd     = if (next.canDrawPreviews) Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next))) else Cmd.None
          (next, cmd)
        case None => (model, Cmd.None)
      }
    case BuildsGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawAllBuildPreviews(model: BuildsGalleryModel): IO[Unit] =
    model.builds.getOrElse(Nil).foldLeft(IO.unit)((acc, build) =>
      acc.flatMap(_ => drawBuildPreview(build, model.buildConfigs.getOrElse(Nil), model.images.getOrElse(Nil), model.palettes.getOrElse(Nil)))
    )

  private def drawPreviewsForCurrentPage(model: BuildsGalleryModel): IO[Unit] = {
    val builds = model.builds.getOrElse(Nil)
    if (builds.isEmpty) IO.unit
    else {
      val pageSize = GalleryLayout.defaultPageSize
      val start    = (model.currentPage - 1) * pageSize
      val slice    = builds.slice(start, start + pageSize)
      val configs  = model.buildConfigs.getOrElse(Nil)
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      slice.foldLeft(IO.unit)((acc, build) =>
        acc.flatMap(_ => drawBuildPreview(build, configs, images, palettes))
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
        CanvasUtils.drawAfterViewReadyDelayed(s"builds-preview-${build.id}", 1, 100, 3)((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
          ctx.fillStyle = "#eee"
          ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
          ctx.fillStyle = "#999"
          ctx.font = "12px \"Press Start 2P\", cursive"
          ctx.fillText("Missing config", 8, buildPreviewHeight / 2)
        })
      case Some(stored) =>
        val imgOpt     = images.find(_.id == stored.config.imageRef)
        val paletteOpt = palettes.find(_.id == stored.config.paletteRef)
        val steps      = BuildScreen.stepsForConfig(stored.config)
        val stepIndex  = build.savedStepIndex.getOrElse(0).max(0).min(if (steps.isEmpty) 0 else steps.length - 1)
        val currentStep = if (steps.isEmpty) None else Some(steps(stepIndex))

        CanvasUtils.drawAfterViewReadyDelayed(
          id = s"builds-preview-${build.id}",
          framesToWait = 1,
          maxRetries = 100,
          delayMs = 3
        )((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
          (imgOpt, paletteOpt) match {
            case (Some(img), Some(palette)) =>
              val pic   = clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)
              val gw    = stored.config.grid.width
              val gh    = stored.config.grid.height
              val scale = (buildPreviewWidth.toDouble / gw).min(buildPreviewHeight.toDouble / gh).min(1.0)
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
              pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
                case Some(cropped) =>
                  val cw = (cropped.width * scale).toInt.max(1)
                  val ch = (cropped.height * scale).toInt.max(1)
                  CanvasUtils.drawPixelPic(canvas, ctx, cropped, cw, ch)
                  ctx.strokeStyle = "rgba(255,0,0,0.6)"
                  ctx.lineWidth = 1
                  stored.config.grid.parts.foreach { part =>
                    ctx.strokeRect(part.x * scale, part.y * scale, (part.width * scale).max(1), (part.height * scale).max(1))
                  }
                  currentStep.foreach { case (sx, sy) =>
                    val rx = sx - stored.config.offsetX
                    val ry = sy - stored.config.offsetY
                    ctx.strokeStyle = "rgba(0,200,0,0.9)"
                    ctx.lineWidth = 2
                    ctx.strokeRect(rx * scale, ry * scale, (patchSize * scale).max(1), (patchSize * scale).max(1))
                  }
                case None =>
                  ctx.fillStyle = "#999"
                  ctx.font = "12px \"Press Start 2P\", cursive"
                  ctx.fillText("Grid out of bounds", 8, buildPreviewHeight / 2)
              }
            case _ =>
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
              ctx.fillStyle = "#999"
              ctx.font = "12px \"Press Start 2P\", cursive"
              ctx.fillText("Missing image/palette", 8, buildPreviewHeight / 2)
          }
        })
    }
  }

  def view(model: Model): Html[Msg] = {
    val backBtn  = button(`class` := NesCss.btn, onClick(BuildsGalleryMsg.Back))(GalleryLayout.backButtonLabel("←", "Overview"))
    val nextBtn  = button(`class` := NesCss.btn, onClick(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None)))(GalleryLayout.nextButtonLabel("Next", "→"))
    (model.builds, model.buildConfigs) match {
      case (None, _) | (_, None) =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = false, Some(nextBtn))
      case (Some(builds), Some(configs)) =>
        val bottomSection =
          if (model.showNewBuildDropdown)
            div(`class` := s"${NesCss.container} ${NesCss.containerRounded} dropdown-panel")(
              div(`class` := "dropdown-panel-title")(text("New build from config:")),
              div(`class` := "flex-row flex-row--tight")(
                select(
                  `class` := NesCss.input,
                  style := "min-width: 14rem;",
                  value := model.selectedBuildConfigId.orElse(configs.headOption.map(_.id)).getOrElse(""),
                  onInput(id => BuildsGalleryMsg.SetSelectedBuildConfigId(if (id.isEmpty) configs.headOption.map(_.id).getOrElse("") else id))
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
            button(`class` := NesCss.btnPrimary, onClick(BuildsGalleryMsg.ShowNewBuildDropdown))(text("+ Start new build"))
        val content =
          if (builds.isEmpty)
            if (model.showNewBuildDropdown)
              div(`class` := GalleryLayout.galleryListClass)(bottomSection)
            else
              GalleryEmptyState("No builds yet.", "+ Start new build", BuildsGalleryMsg.ShowNewBuildDropdown)
          else
            paginatedList(
              builds,
              model.currentPage,
              bottomSection,
              b => entryCard(b, configs, model.pendingDeleteId.contains(b.id))
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = false, Some(nextBtn))
    }
  }

  private def paginatedList(
      builds: List[StoredBuild],
      currentPage: Int,
      addAction: Html[Msg],
      entryCard: StoredBuild => Html[Msg]
  ): Html[Msg] = {
    val pageSize   = GalleryLayout.defaultPageSize
    val totalPages = if (builds.isEmpty) 1 else ((builds.size - 1) / pageSize) + 1
    val page       = currentPage.min(totalPages).max(1)
    val start      = (page - 1) * pageSize
    val slice      = builds.slice(start, start + pageSize)
    GalleryLayout.listWithAddActionAndPagination(
      addAction,
      slice.map(entryCard),
      page,
      totalPages,
      BuildsGalleryMsg.PreviousPage,
      BuildsGalleryMsg.NextPage
    )
  }

  private def entryCard(item: StoredBuild, configs: List[StoredBuildConfig], confirmingDelete: Boolean): Html[Msg] = {
    val configOpt   = configs.find(_.id == item.buildConfigRef)
    val configName  = configOpt.map(_.name).getOrElse(item.buildConfigRef)
    val totalSteps  = configOpt.map(c => BuildScreen.stepsForConfig(c.config).size).getOrElse(0)
    val currentStep = item.savedStepIndex.getOrElse(0).max(0).min(if (totalSteps <= 0) 0 else totalSteps - 1)
    val stepLabel   = if (totalSteps <= 0) "Step 0 of 0" else s"Step ${currentStep + 1} of $totalSteps"
    val progressPct = if (totalSteps <= 0) 0.0 else ((currentStep + 1).toDouble / totalSteps * 100).min(100.0)

    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card gallery-card--start")(
      div(onLoad(BuildsGalleryMsg.DrawBuildPreview(item)))(
        canvas(
          id := s"builds-preview-${item.id}",
          width := buildPreviewWidth,
          height := buildPreviewHeight,
          `class` := "gallery-preview-canvas"
        )()
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
        if (confirmingDelete)
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text nes-text")(text(s"Delete \"${item.name}\"?")),
            button(`class` := NesCss.btnError, onClick(BuildsGalleryMsg.ConfirmDelete(item.id)))(text("Yes")),
            button(`class` := NesCss.btn, onClick(BuildsGalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := NesCss.btnPrimary, onClick(BuildsGalleryMsg.ResumeBuild(item)))(text("Resume")),
            button(`class` := NesCss.btnError, onClick(BuildsGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )
  }
}

final case class BuildsGalleryModel(
    builds: Option[List[StoredBuild]],
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    selectedBuildConfigId: Option[String],
    showNewBuildDropdown: Boolean,
    pendingDeleteId: Option[String],
    currentPage: Int
) {
  def canDrawPreviews: Boolean =
    builds.isDefined && buildConfigs.isDefined && images.isDefined && palettes.isDefined
}

enum BuildsGalleryMsg:
  case LoadedBuilds(list: List[StoredBuild])
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
