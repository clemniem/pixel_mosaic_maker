package clemniem.screens

import cats.effect.IO
import clemniem.{
  BuildConfig,
  Color,
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
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*
import org.scalajs.dom.window

import scala.scalajs.js
import scala.concurrent.duration.DurationInt

/** Step-by-step build: one build config, iterate plates then 16×16 cells per plate. Overview + preview + step nav. */
object BuildScreen extends Screen {
  type Model = BuildScreenModel
  type Msg   = BuildScreenMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildId

  private val overviewCanvasId       = "build-overview"
  private val previewCanvasId        = "build-preview"
  private val patchSize              = 16
  private val defaultPatchBackground = "#eeeeee"

  /** Pastel background that contrasts with all palette colors and with white and black: light, muted, in a band that is clearly off-white and off-black. */
  private def pastelBackgroundFromPalette(colors: Vector[Color]): String = {
    if (colors.isEmpty) defaultPatchBackground
    else {
      val n    = colors.size
      val rAvg = colors.map(_.r).sum / n
      val gAvg = colors.map(_.g).sum / n
      val bAvg = colors.map(_.b).sum / n
      val base = 228
      val tilt = 0.14
      def clamp(c: Int): Int = math.max(0, math.min(255, c))
      val r0 = clamp((base + (255 - rAvg) * tilt).round.toInt)
      val g0 = clamp((base + (255 - gAvg) * tilt).round.toInt)
      val b0 = clamp((base + (255 - bAvg) * tilt).round.toInt)
      /* Keep clearly off-white (max ≤ 215) and off-black (min ≥ 185) for contrast with both */
      val minChan = 185
      val maxChan = 215
      val r = math.max(minChan, math.min(maxChan, r0))
      val g = math.max(minChan, math.min(maxChan, g0))
      val b = math.max(minChan, math.min(maxChan, b0))
      Color(r, g, b).toHex
    }
  }

  private def suggestedPatchBackground(model: Model): Option[String] =
    for {
      stored  <- model.buildConfig
      palettes <- model.palettes
      palette  <- palettes.find(_.id == stored.config.paletteRef)
      if palette.colors.nonEmpty
    } yield pastelBackgroundFromPalette(palette.colors)

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
          stepIndex = stepIndex,
          pendingSave = None,
          patchBackgroundColorHex = defaultPatchBackground
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
        val patchBg   = storedBuild.patchBackgroundColorHex.getOrElse(defaultPatchBackground)
        val model = BuildScreenModel(
          buildConfig = None,
          currentBuild = Some(storedBuild),
          images = None,
          palettes = None,
          stepIndex = stepIndex,
          pendingSave = None,
          patchBackgroundColorHex = patchBg
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
        (BuildScreenModel(None, None, None, None, 0, None, defaultPatchBackground), Cmd.Emit(NavigateNext(ScreenId.BuildsId, None)))
    }
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildScreenMsg.LoadedBuildConfigs(list) =>
      val configOpt = model.currentBuild.flatMap(b => list.find(_.id == b.buildConfigRef))
      val steps     = configOpt.map(c => stepsForConfig(c.config)).getOrElse(Vector.empty)
      val stepIndex = if (steps.isEmpty) 0 else model.stepIndex.max(0).min(steps.length - 1)
      val nextBase  = model.copy(buildConfig = configOpt, stepIndex = stepIndex)
      val next = if (nextBase.patchBackgroundColorHex == defaultPatchBackground)
        suggestedPatchBackground(nextBase).fold(nextBase)(bg => nextBase.copy(patchBackgroundColorHex = bg))
      else nextBase
      (next, drawCmd(next))

    case BuildScreenMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      (next, drawCmd(next))

    case BuildScreenMsg.LoadedPalettes(list) =>
      val nextBase = model.copy(palettes = Some(list))
      val next = if (nextBase.patchBackgroundColorHex == defaultPatchBackground)
        suggestedPatchBackground(nextBase).fold(nextBase)(bg => nextBase.copy(patchBackgroundColorHex = bg))
      else nextBase
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
          val updated   = StoredBuild(buildId, buildName, storedConfig.id, Some(model.stepIndex), Some(model.patchBackgroundColorHex))
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

    case BuildScreenMsg.SetPatchBackgroundColor(hex) =>
      (model.copy(patchBackgroundColorHex = Color.normalizeHex(hex, defaultPatchBackground)), drawCmd(model.copy(patchBackgroundColorHex = Color.normalizeHex(hex, defaultPatchBackground))))

    case BuildScreenMsg.SetStacked(value) =>
      (model.copy(stacked = value), drawCmd(model.copy(stacked = value)))

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
              val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, 400, 400, 1.0)
              canvas.width = fit.width
              canvas.height = fit.height
              ctx.clearRect(0, 0, fit.width, fit.height)
              CanvasUtils.drawPixelPic(canvas, ctx, cropped, fit.width, fit.height, 0, 0)
              ctx.strokeStyle = Color.errorStroke.rgba(0.6)
              ctx.lineWidth = 1
              stored.config.grid.parts.foreach { part =>
                ctx.strokeRect(part.x * fit.scale, part.y * fit.scale, (part.width * fit.scale).max(1), (part.height * fit.scale).max(1))
              }
              model.currentStep.foreach { case (sx, sy) =>
                val rx = sx - stored.config.offsetX
                val ry = sy - stored.config.offsetY
                ctx.strokeStyle = Color.highlightStroke.rgba(0.9)
                ctx.lineWidth = 2
                ctx.strokeRect(rx * fit.scale, ry * fit.scale, (patchSize * fit.scale).max(1), (patchSize * fit.scale).max(1))
              }
            case None =>
              CanvasUtils.drawPlaceholder(canvas, ctx, 400, 200, "Grid region out of bounds")
          }
        case _ =>
          CanvasUtils.drawPlaceholder(canvas, ctx, 400, 200, "Loading…")
      }
    })

  /** Colors in patch sorted by count ascending (least to most); each gets its own 16×16 patch drawn. */
  private def colorsByCountAsc(patch: PixelPic): Vector[(Int, Int)] =
    patch.palette.toVector.sortBy(_._2)

  /** Preview: current 16×16 patch split by color (least→most); each color drawn as its own 16×16 patch. Up to 16 colors; 2 cols on small screens, 4 otherwise. */
  private val previewColsSmall = 2
  private val previewColsWide  = 4
  private val previewColsBreakpoint = 600


  private def patchBackgroundRgb(hex: String): (Int, Int, Int) = {
    val h = Color.normalizeHex(hex, defaultPatchBackground)
    def parse(s: String): Int = {
      val n = java.lang.Integer.parseInt(s, 16)
      if (n >= 0 && n <= 255) n else 238
    }
    if (h.length != 7) (238, 238, 238)
    else
      try (parse(h.substring(1, 3)), parse(h.substring(3, 5)), parse(h.substring(5, 7)))
      catch { case _: Exception => (238, 238, 238) }
  }

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
              val (bgR, bgG, bgB) = patchBackgroundRgb(model.patchBackgroundColorHex)

              if (model.stacked) {
                /* Stacked: one cell per color, each cell shows cumulative layers (like PDF). Layer 0 = color 0 only; Layer 1 = colors 0+1; etc. */
                val n      = sortedColors.size.min(16)
                val cols   = if (window.innerWidth <= previewColsBreakpoint) previewColsSmall else previewColsWide
                val rows   = if (n <= 0) 0 else (n + cols - 1) / cols
                val totalW = if (cols <= 0) 1 else cols * cellW + (cols - 1) * gap
                val totalH = if (rows <= 0) 1 else rows * cellH + (rows - 1) * gap
                canvas.width = totalW.max(1)
                canvas.height = totalH.max(1)
                ctx.clearRect(0, 0, canvas.width, canvas.height)
                sortedColors.zipWithIndex.take(16).foreach { case ((paletteIndex, _), layerIdx) =>
                  val col   = layerIdx % cols
                  val row   = layerIdx / cols
                  val ox    = col * (cellW + gap)
                  val oy    = row * (cellH + gap)
                  val colorSet = sortedColors.take(layerIdx + 1).map(_._1).toSet
                  val imgData = ctx.createImageData(cellW, cellH)
                  val data   = imgData.data
                  for (y <- 0 until patchSize; x <- 0 until patchSize) {
                    val idx = y * patchSize + x
                    val (r, g, b) =
                      if (colorSet.contains(patch.pixels(idx))) {
                        val px = patch.paletteLookup(patch.pixels(idx))
                        (px.r, px.g, px.b)
                      } else (bgR, bgG, bgB)
                    for (py <- 0 until cellPx; pxOff <- 0 until cellPx) {
                      val off = ((y * cellPx + py) * cellW + (x * cellPx + pxOff)) * 4
                      data(off) = r
                      data(off + 1) = g
                      data(off + 2) = b
                      data(off + 3) = 255
                    }
                  }
                  ctx.putImageData(imgData, ox, oy)
                  ctx.strokeStyle = Color.black.rgba(0.45)
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
              } else {
                /* Grid: each color in its own cell */
                val n      = sortedColors.size.min(16)
                val cols   = if (window.innerWidth <= previewColsBreakpoint) previewColsSmall else previewColsWide
                val rows   = if (n <= 0) 0 else (n + cols - 1) / cols
                val totalW = if (cols <= 0) 1 else cols * cellW + (cols - 1) * gap
                val totalH = if (rows <= 0) 1 else rows * cellH + (rows - 1) * gap
                canvas.width = totalW.max(1)
                canvas.height = totalH.max(1)
                ctx.clearRect(0, 0, canvas.width, canvas.height)
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
                  ctx.strokeStyle = Color.black.rgba(0.45)
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
              }
            case None =>
              canvas.width = patchSize
              canvas.height = patchSize
              val (r, g, b) = patchBackgroundRgb(model.patchBackgroundColorHex)
              ctx.fillStyle = s"rgb($r,$g,$b)"
              ctx.fillRect(0, 0, patchSize, patchSize)
          }
        case None =>
          canvas.width = patchSize
          canvas.height = patchSize
          val (r, g, b) = patchBackgroundRgb(model.patchBackgroundColorHex)
          ctx.fillStyle = s"rgb($r,$g,$b)"
          ctx.fillRect(0, 0, patchSize, patchSize)
      }
    })

  def view(model: Model): Html[Msg] = {
    val steps   = model.steps
    val total   = steps.size
    val current = model.stepIndex
    val title   = model.currentBuild.map(_.name).orElse(model.buildConfig.map(_.name)).getOrElse("Build")

    div(`class` := s"${NesCss.screenContainer} screen-container--wide")(
      ScreenHeader(
        title,
        div(
          div(`class` := "flex-row")( /* row 0: Save + Back */
            button(
              `class` := (if (model.pendingSave.isDefined) s"${NesCss.btn} btn-disabled" else NesCss.btnSuccess),
              onClick(BuildScreenMsg.Save)
            )(text(if (model.pendingSave.isDefined) "Saving…" else "Save step")),
            GalleryLayout.backButton(BuildScreenMsg.Back, "Builds")
          ),
          div(`class` := "flex-row flex-row--tight", style := "margin-top: 0.5rem;")(
            button(
              `class` := (if (total == 0 || current <= 0) s"${NesCss.btn} btn-disabled" else NesCss.btn),
              onClick(BuildScreenMsg.PrevStep)
            )(text("Previous")),
            button(
              `class` := (if (total == 0 || current >= total - 1) s"${NesCss.btn} btn-disabled" else NesCss.btn),
              onClick(BuildScreenMsg.NextStep)
            )(text("Next")),
            input(
              `type` := "number",
              min := "1",
              max := total.max(1).toString,
              value := (if (total == 0) "0" else (current + 1).toString),
              onInput(s => BuildScreenMsg.SetStep(s.toIntOption.getOrElse(1) - 1)),
              `class` := s"${NesCss.input} input-w-4"
            ),
            text(" from "),
            span(`class` := "section-title", style := "margin: 0; align-self: center;")(text(total.toString))
          )
        ),
        None,
        false
      ),
      div(`class` := "build-overview-row")(
        div(`class` := "build-config-canvas-block")(
          div(`class` := "section-title")(text("Overview")),
          div(onLoad(BuildScreenMsg.Draw))(
            canvas(id := overviewCanvasId, width := 400, height := 200, `class` := "pixel-canvas")()
          )
        ),
        div(`class` := s"${NesCss.field} build-patch-bg-block")(
          label(`class` := "label-block")(text("Background")),
          input(
            `type` := "color",
            `class` := "input-color",
            value := Color.normalizeHex(model.patchBackgroundColorHex, defaultPatchBackground),
            onInput(hex => BuildScreenMsg.SetPatchBackgroundColor(hex))
          )
        ),
        div(`class` := s"${NesCss.field} build-stacked-block")(
          label(`class` := "label-block")(text("Stacked")),
          div(`class` := "stacked-radios")(
            label(`class` := "stacked-radio-option")(
              input(
                `type`  := "radio",
                `class` := NesCss.radio,
                name    := "stacked",
                value   := "off",
                checked := !model.stacked,
                onClick(BuildScreenMsg.SetStacked(false))
              ),
              span(text("Off"))
            ),
            label(`class` := "stacked-radio-option")(
              input(
                `type`  := "radio",
                `class` := NesCss.radio,
                name    := "stacked",
                value   := "on",
                checked := model.stacked,
                onClick(BuildScreenMsg.SetStacked(true))
              ),
              span(text("On"))
            )
          )
        )
      ),
      div(`class` := "build-preview-row")(
        div(`class` := "build-preview-header")(
          div(`class` := "section-title")(text("Step by color"))
        ),
        div(`class` := "build-preview-inner", onLoad(BuildScreenMsg.Draw))(
          canvas(id := previewCanvasId, width := 32, height := 32, `class` := "pixel-canvas")()
        )
      )
    )
  }

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.every[IO](300.millis, "build-resize").map(_ => BuildScreenMsg.Draw)
}

final case class BuildScreenModel(
    buildConfig: Option[StoredBuildConfig],
    currentBuild: Option[StoredBuild],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    stepIndex: Int,
    pendingSave: Option[StoredBuild] = None,
    patchBackgroundColorHex: String = "#eeeeee",
    stacked: Boolean = false
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
  case SetPatchBackgroundColor(hex: String)
  case SetStacked(value: Boolean)