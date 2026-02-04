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
    val model = BuildsGalleryModel(None, None, None, None, None, showNewBuildDropdown = false)
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
      val valid = list.filter(_.buildConfigRef.nonEmpty)
      val next  = model.copy(builds = Some(valid))
      val cmd   = if (next.canDrawPreviews) Cmd.SideEffect(drawAllBuildPreviews(next)) else Cmd.None
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
    case BuildsGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawAllBuildPreviews(model: BuildsGalleryModel): IO[Unit] =
    model.builds.getOrElse(Nil).foldLeft(IO.unit)((acc, build) =>
      acc.flatMap(_ => drawBuildPreview(build, model.buildConfigs.getOrElse(Nil), model.images.getOrElse(Nil), model.palettes.getOrElse(Nil)))
    )

  private def drawBuildPreview(
      build: StoredBuild,
      configs: List[StoredBuildConfig],
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): IO[Unit] = {
    val configOpt = configs.find(_.id == build.buildConfigRef)
    configOpt match {
      case None =>
        CanvasUtils.drawAfterViewReadyDelayed(s"builds-preview-${build.id}", 2, 100, 3)((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
          canvas.width = buildPreviewWidth
          canvas.height = buildPreviewHeight
          ctx.fillStyle = "#eee"
          ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
          ctx.fillStyle = "#999"
          ctx.font = "12px system-ui"
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
          framesToWait = 2,
          maxRetries = 100,
          delayMs = 3
        )((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
          (imgOpt, paletteOpt) match {
            case (Some(img), Some(palette)) =>
              val pic   = clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)
              val gw    = stored.config.grid.width
              val gh    = stored.config.grid.height
              val scale = (buildPreviewWidth.toDouble / gw).min(buildPreviewHeight.toDouble / gh).min(1.0)
              pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
                case Some(cropped) =>
                  val cw = (cropped.width * scale).toInt.max(1)
                  val ch = (cropped.height * scale).toInt.max(1)
                  canvas.width = cw
                  canvas.height = ch
                  ctx.clearRect(0, 0, cw, ch)
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
                  canvas.width = buildPreviewWidth
                  canvas.height = buildPreviewHeight
                  ctx.fillStyle = "#eee"
                  ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
                  ctx.fillStyle = "#999"
                  ctx.font = "12px system-ui"
                  ctx.fillText("Grid out of bounds", 8, buildPreviewHeight / 2)
              }
            case _ =>
              canvas.width = buildPreviewWidth
              canvas.height = buildPreviewHeight
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, buildPreviewWidth, buildPreviewHeight)
              ctx.fillStyle = "#999"
              ctx.font = "12px system-ui"
              ctx.fillText("Missing image/palette", 8, buildPreviewHeight / 2)
          }
        })
    }
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
    val configOpt   = configs.find(_.id == item.buildConfigRef)
    val configName  = configOpt.map(_.name).getOrElse(item.buildConfigRef)
    val totalSteps  = configOpt.map(c => BuildScreen.stepsForConfig(c.config).size).getOrElse(0)
    val currentStep = item.savedStepIndex.getOrElse(0).max(0).min(if (totalSteps <= 0) 0 else totalSteps - 1)
    val stepLabel   = if (totalSteps <= 0) "Step 0 of 0" else s"Step ${currentStep + 1} of $totalSteps"
    val progressPct = if (totalSteps <= 0) 0.0 else ((currentStep + 1).toDouble / totalSteps * 100).min(100.0)

    div(
      style := "display: flex; align-items: flex-start; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(onLoad(BuildsGalleryMsg.DrawBuildPreview(item)))(
        canvas(
          id := s"builds-preview-${item.id}",
          width := buildPreviewWidth,
          height := buildPreviewHeight,
          style := "border: 1px solid #999; border-radius: 4px; flex-shrink: 0; image-rendering: pixelated; image-rendering: crisp-edges;"
        )()
      ),
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(configName)
        ),
        div(style := "margin-top: 0.5rem;")(
          span(style := "font-size: 0.8125rem; color: #555;")(text(stepLabel)),
          div(
            style := "margin-top: 4px; height: 8px; background: #e0e0e0; border-radius: 4px; overflow: hidden;"
          )(
            div(
              style := s"height: 100%; width: ${progressPct}%; background: #2e7d32; border-radius: 4px; transition: width 0.2s;"
            )()
          )
        ),
        button(
          style := "margin-top: 0.5rem; padding: 4px 10px; cursor: pointer; flex-shrink: 0;",
          onClick(BuildsGalleryMsg.ResumeBuild(item))
        )(text("Resume"))
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
    showNewBuildDropdown: Boolean
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
  case Back
