package clemniem.screens

import cats.effect.IO
import clemniem.{
  GridConfig,
  NavigateNext,
  PixelPic,
  Screen,
  ScreenId,
  StoredBuildConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, LocalStorageUtils, PdfUtils, PrintBookRequest}
import clemniem.StorageKeys
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Screen to generate the PDF print instructions: choose a BuildConfig and set options (e.g. title). */
object PrintInstructionsScreen extends Screen {
  type Model = PrintInstructionsModel
  type Msg   = PrintInstructionsMsg | NavigateNext

  val screenId: ScreenId       = ScreenId.PrintInstructionsId
  private val overviewCanvasId = "print-instructions-overview"

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = PrintInstructionsModel(
      buildConfigs = None,
      images = None,
      palettes = None,
      selectedBuildConfigId = None,
      title = "Mosaic",
      stepSizePx = 16
    )
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      PrintInstructionsMsg.LoadedBuildConfigs.apply,
      _ => PrintInstructionsMsg.LoadedBuildConfigs(Nil),
      (_, _) => PrintInstructionsMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      PrintInstructionsMsg.LoadedImages.apply,
      _ => PrintInstructionsMsg.LoadedImages(Nil),
      (_, _) => PrintInstructionsMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      PrintInstructionsMsg.LoadedPalettes.apply,
      _ => PrintInstructionsMsg.LoadedPalettes(Nil),
      (_, _) => PrintInstructionsMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuildConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PrintInstructionsMsg.LoadedBuildConfigs(list) =>
      val selectedId = model.selectedBuildConfigId.orElse(list.headOption.map(_.id))
      val next       = model.copy(buildConfigs = Some(list), selectedBuildConfigId = selectedId)
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.SetBuildConfig(id) =>
      val next = model.copy(selectedBuildConfigId = Some(id))
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.DrawOverview =>
      (model, Cmd.SideEffect(drawOverview(model)))
    case PrintInstructionsMsg.SetTitle(title) =>
      (model.copy(title = title), Cmd.None)
    case PrintInstructionsMsg.SetStepSize(px) =>
      (model.copy(stepSizePx = px), Cmd.None)
    case PrintInstructionsMsg.PrintPdf =>
      val request = PrintBookRequest(
        title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
        mosaicPicAndGridOpt = model.selectedStored.flatMap(stored =>
          mosaicPicAndGridForStored(stored, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))
        ),
        stepSizePx = model.stepSizePx
      )
      (model, Cmd.SideEffect(PdfUtils.printBookPdf(request)))
    case PrintInstructionsMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container = "font-family: system-ui, sans-serif; max-width: 36rem; margin: 0 auto; padding: 1.5rem;"
    val buildConfigs = model.buildConfigs.getOrElse(Nil)
    val selectedId   = model.selectedBuildConfigId.orElse(buildConfigs.headOption.map(_.id))
    val canPrint     = model.selectedStored.isDefined

    div(style := container)(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; flex-wrap: wrap; gap: 8px;")(
        h2(style := "margin: 0;")(text("Print Instructions")),
        button(
          style := "padding: 6px 12px; cursor: pointer; border: 1px solid #555; border-radius: 4px; background: #fff;",
          onClick(PrintInstructionsMsg.Back)
        )(text("← Overview"))
      ),
      p(style := "color: #555; margin-bottom: 1.5rem;")(
        text("Choose a build config and set the booklet title, then generate the PDF.")
      ),
      div(style := "margin-bottom: 1rem;")(
        label(style := "display: block; font-weight: 500; margin-bottom: 0.35rem;")(text("Build config")),
        model.buildConfigs match {
          case None =>
            span(style := "color: #666;")(text("Loading…"))
          case Some(list) if list.isEmpty =>
            span(style := "color: #666;")(text("No build configs saved. Create one from the BuildConfigs gallery."))
          case Some(list) =>
            select(
              style := "padding: 6px 10px; min-width: 16rem; border: 1px solid #ccc; border-radius: 4px;",
              value := selectedId.getOrElse(""),
              onInput(s => PrintInstructionsMsg.SetBuildConfig(if (s.isEmpty) list.headOption.map(_.id).getOrElse("") else s))
            )(
              list.map { item =>
                option(value := item.id)(text(item.name))
              }*
            )
        }
      ),
      model.buildConfigs match {
        case Some(_) =>
          div(style := "margin-bottom: 1.5rem;")(
            div(style := "margin-bottom: 0.5rem; font-weight: 500;")(text("Grid overview")),
            div(onLoad(PrintInstructionsMsg.DrawOverview))(
              canvas(
                id := overviewCanvasId,
                width := 400,
                height := 200,
                style := "border: 1px solid #333; display: block; max-width: 100%; image-rendering: pixelated; image-rendering: crisp-edges;"
              )()
            )
          )
        case None =>
          div()()
      },
      div(style := "margin-bottom: 1.5rem;")(
        label(style := "display: block; font-weight: 500; margin-bottom: 0.35rem;")(text("Title")),
        input(
          `type` := "text",
          value := model.title,
          onInput(PrintInstructionsMsg.SetTitle.apply),
          style := "padding: 6px 10px; width: 100%; max-width: 20rem; border: 1px solid #ccc; border-radius: 4px; box-sizing: border-box;"
        )
      ),
      div(style := "margin-bottom: 1.5rem;")(
        label(style := "display: block; font-weight: 500; margin-bottom: 0.35rem;")(text("Step size (px)")),
        input(
          `type` := "number",
          value := model.stepSizePx.toString,
          min := "4",
          max := "64",
          onInput(s => PrintInstructionsMsg.SetStepSize(parseStepSize(s))),
          style := "padding: 6px 10px; width: 5rem; border: 1px solid #ccc; border-radius: 4px; box-sizing: border-box;"
        ),
        span(style := "margin-left: 0.5rem; color: #555; font-size: 0.9rem;")(text("Each step = step×step px. Plate width/height must be divisible by this (default 16)."))
      ),
      button(
        style := (if (!canPrint)
          "padding: 8px 16px; cursor: not-allowed; opacity: 0.6; background: #999; color: #fff; border: none; border-radius: 4px; font-weight: 500;"
        else
          "padding: 8px 16px; cursor: pointer; background: #1565c0; color: #fff; border: none; border-radius: 4px; font-weight: 500;"),
        onClick(PrintInstructionsMsg.PrintPdf)
      )(text("Print PDF"))
    )
  }

  /** Full image with palette applied (for overview canvas). */
  private def fullPicForStored(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): Option[PixelPic] =
    for {
      img     <- images.find(_.id == stored.config.imageRef)
      palette <- palettes.find(_.id == stored.config.paletteRef)
    } yield clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)

  private def drawOverview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      model.selectedStored.flatMap(stored =>
        fullPicForStored(stored, images, palettes).map(pic => (stored, pic))
      ) match {
        case Some((stored, pic)) =>
          drawFullImageWithGrid(canvas, ctx, pic, stored.config.grid, stored.config.offsetX, stored.config.offsetY)
        case None =>
          drawPlaceholder(canvas, ctx, 400, 200, "Select a build config for overview")
      }
    })

  private def drawFullImageWithGrid(
      canvas: Canvas,
      ctx: CanvasRenderingContext2D,
      pic: PixelPic,
      grid: GridConfig,
      offsetX: Int,
      offsetY: Int
  ): Unit = {
    val scale = (400.0 / (pic.width.max(pic.height))).min(1.0)
    val cw    = (pic.width * scale).toInt.max(1)
    val ch    = (pic.height * scale).toInt.max(1)
    canvas.width = cw
    canvas.height = ch
    ctx.clearRect(0, 0, cw, ch)
    CanvasUtils.drawPixelPic(canvas, ctx, pic, cw, ch)
    ctx.strokeStyle = "rgba(255,0,0,0.8)"
    ctx.lineWidth = 1
    val ox  = (offsetX * scale).toInt
    val oy  = (offsetY * scale).toInt
    val gsx = scale
    val gsy = scale
    grid.parts.foreach { part =>
      ctx.strokeRect(ox + part.x * gsx, oy + part.y * gsy, (part.width * gsx).max(1), (part.height * gsy).max(1))
    }
  }

  private def drawPlaceholder(canvas: Canvas, ctx: CanvasRenderingContext2D, w: Int, h: Int, text: String): Unit = {
    canvas.width = w
    canvas.height = h
    ctx.fillStyle = "#eee"
    ctx.fillRect(0, 0, w, h)
    ctx.fillStyle = "#999"
    ctx.font = "14px system-ui"
    ctx.fillText(text, 12, h / 2)
  }

  private def mosaicPicAndGridForStored(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): Option[(PixelPic, GridConfig)] =
    for {
      img     <- images.find(_.id == stored.config.imageRef)
      palette <- palettes.find(_.id == stored.config.paletteRef)
      pic     = clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)
      gw      = stored.config.grid.width
      gh      = stored.config.grid.height
      cropped <- pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh)
    } yield (cropped, stored.config.grid)
}

private def parseStepSize(s: String): Int = {
  val n = s.trim.toIntOption.getOrElse(16)
  n.max(4).min(64)
}

final case class PrintInstructionsModel(
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    selectedBuildConfigId: Option[String],
    title: String,
    stepSizePx: Int
) {
  def selectedStored: Option[StoredBuildConfig] =
    buildConfigs.flatMap(list =>
      selectedBuildConfigId.flatMap(id => list.find(_.id == id))
    )
}

enum PrintInstructionsMsg:
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case SetBuildConfig(id: String)
  case SetTitle(title: String)
  case SetStepSize(px: Int)
  case DrawOverview
  case PrintPdf
  case Back
