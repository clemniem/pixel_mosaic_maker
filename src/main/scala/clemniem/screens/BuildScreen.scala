package clemniem.screens

import cats.effect.IO
import clemniem.{
  BuildConfig,
  NavigateNext,
  PixelPic,
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
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Step-by-step build: one build config, iterate plates then 16×16 cells per plate. Overview + preview + step nav. */
object BuildScreen extends Screen {
  type Model = BuildScreenModel
  type Msg   = BuildScreenMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildId

  private val overviewCanvasId = "build-overview"
  private val previewCanvasId  = "build-preview"
  private val patchSize        = 16

  /** One step = one 16×16 patch in image coordinates (top-left). */
  def stepsForConfig(config: BuildConfig): Vector[(Int, Int)] = {
    val grid = config.grid
    val ox   = config.offsetX
    val oy   = config.offsetY
    val out  = Vector.newBuilder[(Int, Int)]
    for (part <- grid.parts) {
      val nCols = part.width / patchSize
      val nRows = part.height / patchSize
      for (cy <- 0 until nRows; cx <- 0 until nCols) {
        val imgX = ox + part.x + cx * patchSize
        val imgY = oy + part.y + cy * patchSize
        out += ((imgX, imgY))
      }
    }
    out.result()
  }

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    previous match {
      case Some(ScreenOutput.StartBuild(storedConfig)) =>
        val stepIndex = 0
        val model = BuildScreenModel(
          buildConfig = Some(storedConfig),
          currentBuild = None,
          images = None,
          palettes = None,
          stepIndex = stepIndex
        )
        val loadImages   = LocalStorageUtils.loadList(StorageKeys.images)(
          BuildScreenMsg.LoadedImages.apply,
          _ => BuildScreenMsg.LoadedImages(Nil),
          (_, _) => BuildScreenMsg.LoadedImages(Nil)
        )
        val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
          BuildScreenMsg.LoadedPalettes.apply,
          _ => BuildScreenMsg.LoadedPalettes(Nil),
          (_, _) => BuildScreenMsg.LoadedPalettes(Nil)
        )
        (model, Cmd.Batch(loadImages, loadPalettes))

      case Some(ScreenOutput.ResumeBuild(storedBuild)) =>
        val stepIndex = storedBuild.savedStepIndex.getOrElse(0)
        val model = BuildScreenModel(
          buildConfig = None,
          currentBuild = Some(storedBuild),
          images = None,
          palettes = None,
          stepIndex = stepIndex
        )
        val loadConfigs  = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
          BuildScreenMsg.LoadedBuildConfigs.apply,
          _ => BuildScreenMsg.LoadedBuildConfigs(Nil),
          (_, _) => BuildScreenMsg.LoadedBuildConfigs(Nil)
        )
        val loadImages   = LocalStorageUtils.loadList(StorageKeys.images)(
          BuildScreenMsg.LoadedImages.apply,
          _ => BuildScreenMsg.LoadedImages(Nil),
          (_, _) => BuildScreenMsg.LoadedImages(Nil)
        )
        val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
          BuildScreenMsg.LoadedPalettes.apply,
          _ => BuildScreenMsg.LoadedPalettes(Nil),
          (_, _) => BuildScreenMsg.LoadedPalettes(Nil)
        )
        (model, Cmd.Batch(loadConfigs, loadImages, loadPalettes))

      case _ =>
        (BuildScreenModel(None, None, None, None, 0, None), Cmd.Emit(NavigateNext(ScreenId.BuildsId, None)))
    }
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildScreenMsg.LoadedBuildConfigs(list) =>
      val configOpt = model.currentBuild.flatMap(b => list.find(_.id == b.buildConfigRef))
      val steps     = configOpt.map(c => stepsForConfig(c.config)).getOrElse(Vector.empty)
      val stepIndex = if (steps.isEmpty) 0 else model.stepIndex.max(0).min(steps.length - 1)
      val next = model.copy(
        buildConfig = configOpt,
        stepIndex = stepIndex
      )
      (next, drawCmd(next))

    case BuildScreenMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      (next, drawCmd(next))

    case BuildScreenMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      (next, drawCmd(next))

    case BuildScreenMsg.SetStep(index) =>
      val steps = model.steps
      val idx   = index.max(0).min(if (steps.isEmpty) 0 else steps.length - 1)
      (model.copy(stepIndex = idx), drawCmd(model.copy(stepIndex = idx)))

    case BuildScreenMsg.PrevStep =>
      val idx = (model.stepIndex - 1).max(0)
      (model.copy(stepIndex = idx), drawCmd(model.copy(stepIndex = idx)))

    case BuildScreenMsg.NextStep =>
      val steps = model.steps
      val idx   = if (steps.isEmpty) 0 else (model.stepIndex + 1).min(steps.length - 1)
      (model.copy(stepIndex = idx), drawCmd(model.copy(stepIndex = idx)))

    case BuildScreenMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildsId, None)))

    case BuildScreenMsg.Save =>
      (model.buildConfig, model.currentBuild) match {
        case (Some(storedConfig), _) =>
          val buildId   = model.currentBuild.map(_.id).getOrElse("build-" + js.Date.now().toLong)
          val buildName = model.currentBuild.map(_.name).getOrElse(storedConfig.name)
          val updated   = StoredBuild(buildId, buildName, storedConfig.id, Some(model.stepIndex))
          val cmd = LocalStorageUtils.loadList(StorageKeys.builds)(
            list => BuildScreenMsg.LoadedForSave(list),
            _ => BuildScreenMsg.LoadedForSave(Nil),
            (_, _) => BuildScreenMsg.SaveFailed
          )
          (model.copy(pendingSave = Some(updated)), cmd)
        case _ =>
          (model, Cmd.None)
      }

    case BuildScreenMsg.LoadedForSave(list) =>
      model.pendingSave match {
        case Some(updated) =>
          val newList = model.currentBuild match {
            case Some(existing) if list.exists(_.id == existing.id) =>
              list.map(item => if (item.id == updated.id) updated else item)
            case _ =>
              list :+ updated
          }
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.builds, newList)(
            _ => BuildScreenMsg.SaveDone,
            (_, _) => BuildScreenMsg.SaveFailed
          )
          (model.copy(pendingSave = None, currentBuild = Some(updated)), saveCmd)
        case None =>
          (model.copy(pendingSave = None), Cmd.None)
      }

    case BuildScreenMsg.SaveDone =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildsId, None)))

    case BuildScreenMsg.SaveFailed =>
      (model.copy(pendingSave = None), Cmd.None)

    case BuildScreenMsg.Draw =>
      (model, drawCmd(model))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawCmd(model: Model): Cmd[IO, Msg] =
    Cmd.SideEffect(drawOverview(model).flatMap(_ => drawPreview(model)))

  private def picWithPalette(model: Model): Option[PixelPic] =
    for {
      stored  <- model.buildConfig
      img     <- model.images.flatMap(_.find(_.id == stored.config.imageRef))
      palette <- model.palettes.flatMap(_.find(_.id == stored.config.paletteRef))
    } yield clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)

  /** Overview: grid region only (cropped image + grid overlay + current 16×16 patch highlighted). */
  private def drawOverview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val picOpt = picWithPalette(model)
      val cfgOpt = model.buildConfig
      (picOpt, cfgOpt) match {
        case (Some(pic), Some(stored)) =>
          val gw = stored.config.grid.width
          val gh = stored.config.grid.height
          pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
            case Some(cropped) =>
              val scale = (400.0 / (cropped.width.max(cropped.height))).min(1.0)
              val cw   = (cropped.width * scale).toInt.max(1)
              val ch   = (cropped.height * scale).toInt.max(1)
              canvas.width = cw
              canvas.height = ch
              ctx.clearRect(0, 0, cw, ch)
              CanvasUtils.drawPixelPic(canvas, ctx, cropped, cw, ch)
              ctx.strokeStyle = "rgba(255,0,0,0.6)"
              ctx.lineWidth = 1
              val gsx = scale
              val gsy = scale
              stored.config.grid.parts.foreach { part =>
                ctx.strokeRect(part.x * gsx, part.y * gsy, (part.width * gsx).max(1), (part.height * gsy).max(1))
              }
              model.currentStep.foreach { case (sx, sy) =>
                val rx = sx - stored.config.offsetX
                val ry = sy - stored.config.offsetY
                ctx.strokeStyle = "rgba(0,200,0,0.9)"
                ctx.lineWidth = 2
                ctx.strokeRect(rx * scale, ry * scale, (patchSize * scale).max(1), (patchSize * scale).max(1))
              }
            case None =>
              canvas.width = 400
              canvas.height = 200
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, 400, 200)
              ctx.fillStyle = "#999"
              ctx.font = "14px system-ui"
              ctx.fillText("Grid region out of bounds", 12, 100)
          }
        case _ =>
          canvas.width = 400
          canvas.height = 200
          ctx.fillStyle = "#eee"
          ctx.fillRect(0, 0, 400, 200)
          ctx.fillStyle = "#999"
          ctx.font = "14px system-ui"
          ctx.fillText("Loading…", 12, 100)
      }
    })

  /** Colors in patch sorted by count ascending (least to most); each gets its own 16×16 patch drawn. */
  private def colorsByCountAsc(patch: PixelPic): Vector[(Int, Int)] =
    patch.palette.toVector.sortBy(_._2)

  /** Preview: current 16×16 patch split by color (least→most); each color drawn as its own 16×16 patch. Up to 16 colors in a 4-column grid. */
  private val previewCols = 4

  private def drawPreview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(previewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val picOpt = picWithPalette(model)
      model.currentStep match {
        case Some((sx, sy)) =>
          picOpt.flatMap(_.crop(sx, sy, patchSize, patchSize)) match {
            case Some(patch) =>
              val sortedColors = colorsByCountAsc(patch)
              val cellPx       = 8
              val cellW        = patchSize * cellPx
              val cellH        = patchSize * cellPx
              val gap          = 8
              val gridStep     = 4
              val n            = sortedColors.size.min(16)
              val cols         = previewCols
              val rows         = if (n <= 0) 0 else (n + cols - 1) / cols
              val totalW       = if (cols <= 0) 1 else cols * cellW + (cols - 1) * gap
              val totalH       = if (rows <= 0) 1 else rows * cellH + (rows - 1) * gap
              canvas.width = totalW.max(1)
              canvas.height = totalH.max(1)
              ctx.clearRect(0, 0, canvas.width, canvas.height)
              val bgR = 238
              val bgG = 238
              val bgB = 238
              sortedColors.zipWithIndex.take(16).foreach { case ((paletteIndex, _), i) =>
                val px  = patch.paletteLookup(paletteIndex)
                val col = i % cols
                val row = i / cols
                val ox  = col * (cellW + gap)
                val oy  = row * (cellH + gap)
                val imgData = ctx.createImageData(cellW, cellH)
                val data    = imgData.data
                for (y <- 0 until patchSize; x <- 0 until patchSize) {
                  val idx = y * patchSize + x
                  val (r, g, b) =
                    if (patch.pixels(idx) == paletteIndex) (px.r, px.g, px.b)
                    else (bgR, bgG, bgB)
                  for (py <- 0 until cellPx; pxOff <- 0 until cellPx) {
                    val off = ((y * cellPx + py) * cellW + (x * cellPx + pxOff)) * 4
                    data(off) = r
                    data(off + 1) = g
                    data(off + 2) = b
                    data(off + 3) = 255
                  }
                }
                ctx.putImageData(imgData, ox, oy)
                ctx.strokeStyle = "rgba(0,0,0,0.45)"
                ctx.lineWidth = 1
                for (g <- 1 until 4) {
                  val pos = g * gridStep * cellPx
                  ctx.beginPath()
                  ctx.moveTo(ox + pos, oy)
                  ctx.lineTo(ox + pos, oy + cellH)
                  ctx.stroke()
                  ctx.beginPath()
                  ctx.moveTo(ox, oy + pos)
                  ctx.lineTo(ox + cellW, oy + pos)
                  ctx.stroke()
                }
              }
            case None =>
              canvas.width = patchSize
              canvas.height = patchSize
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, patchSize, patchSize)
          }
        case None =>
          canvas.width = patchSize
          canvas.height = patchSize
          ctx.fillStyle = "#eee"
          ctx.fillRect(0, 0, patchSize, patchSize)
      }
    })

  def view(model: Model): Html[Msg] = {
    val container = "font-family: system-ui, sans-serif; max-width: 42rem; margin: 0 auto; padding: 1rem;"
    val steps    = model.steps
    val total    = steps.size
    val current  = model.stepIndex
    val stepLabel = if (total == 0) "Step 0 / 0" else s"Step ${current + 1} / $total"

    div(style := container)(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem; flex-wrap: wrap; gap: 8px;")(
        h2(style := "margin: 0;")(text(model.currentBuild.map(_.name).orElse(model.buildConfig.map(_.name)).getOrElse("Build"))),
        div(style := "display: flex; align-items: center; gap: 8px;")(
          button(
            style := "padding: 6px 14px; cursor: pointer; background: #2e7d32; color: #fff; border: none; border-radius: 4px; font-weight: 500;",
            onClick(BuildScreenMsg.Save)
          )(text(if (model.pendingSave.isDefined) "Saving…" else "Save step")),
          button(style := "padding: 6px 12px; cursor: pointer;", onClick(BuildScreenMsg.Back))(text("← Builds"))
        )
      ),
      div(style := "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem; flex-wrap: wrap;")(
        span(style := "font-weight: 500;")(text(stepLabel)),
        button(
          style := (if (total == 0 || current <= 0) "padding: 4px 10px; cursor: not-allowed; opacity: 0.6;" else "padding: 4px 10px; cursor: pointer;"),
          onClick(BuildScreenMsg.PrevStep)
        )(text("Previous")),
        button(
          style := (if (total == 0 || current >= total - 1) "padding: 4px 10px; cursor: not-allowed; opacity: 0.6;" else "padding: 4px 10px; cursor: pointer;"),
          onClick(BuildScreenMsg.NextStep)
        )(text("Next")),
        label(style := "display: flex; align-items: center; gap: 0.5rem;")(
          text("Go to:"),
          input(
            `type` := "number",
            min := "1",
            max := total.max(1).toString,
            value := (if (total == 0) "0" else (current + 1).toString),
            onInput(s => BuildScreenMsg.SetStep(s.toIntOption.getOrElse(1) - 1)),
            style := "width: 4rem; padding: 4px;"
          )
        )
      ),
      div(style := "margin-top: 1rem; display: flex; flex-wrap: wrap; gap: 1rem; align-items: flex-start;")(
        div(style := "flex: 1; min-width: 200px;")(
          div(style := "margin-bottom: 0.5rem; font-weight: 500;")(text("Overview")),
          div(onLoad(BuildScreenMsg.Draw))(
            canvas(
              id := overviewCanvasId,
              width := 400,
              height := 200,
              style := "border: 1px solid #333; display: block; max-width: 100%; image-rendering: pixelated; image-rendering: crisp-edges;"
            )()
          )
        ),
        div(style := "flex: 0 0 auto; max-width: 100%;")(
          div(style := "margin-bottom: 0.5rem; font-weight: 500;")(text("Current patch by color (least → most)")),
          div(style := "overflow: auto; display: inline-block; max-height: 70vh;", onLoad(BuildScreenMsg.Draw))(
            canvas(
              id := previewCanvasId,
              width := 32,
              height := 32,
              style := "display: block; image-rendering: pixelated; image-rendering: crisp-edges; min-height: 128px;"
            )()
          )
        )
      )
    )
  }
}

final case class BuildScreenModel(
    buildConfig: Option[StoredBuildConfig],
    currentBuild: Option[StoredBuild],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    stepIndex: Int,
    pendingSave: Option[StoredBuild] = None
) {
  def steps: Vector[(Int, Int)] =
    buildConfig match {
      case Some(stored) => BuildScreen.stepsForConfig(stored.config)
      case None          => Vector.empty
    }

  def currentStep: Option[(Int, Int)] = {
    val s = steps
    if (s.isEmpty || stepIndex < 0 || stepIndex >= s.length) None
    else Some(s(stepIndex))
  }
}

enum BuildScreenMsg:
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case SetStep(index: Int)
  case PrevStep
  case NextStep
  case Back
  case Save
  case LoadedForSave(list: List[StoredBuild])
  case SaveDone
  case SaveFailed
  case Draw
