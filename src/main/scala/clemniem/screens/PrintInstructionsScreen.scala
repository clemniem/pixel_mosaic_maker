package clemniem.screens

import cats.effect.IO
import clemniem.{
  Color,
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
import clemniem.common.nescss.NesCss
import clemniem.StorageKeys
import tyrian.Html.*
import tyrian.*

/** Fixed step size options (px). Only those that divide every plate width/height are shown. Default 16 if available else smallest. */
private val stepSizeCandidates = List(12, 16, 20, 24, 28, 32)

private def availableStepSizesForGrid(grid: GridConfig): List[Int] =
  stepSizeCandidates.filter(s => grid.parts.forall(p => p.width % s == 0 && p.height % s == 0))

private def defaultStepSizeForAvailable(available: List[Int]): Int =
  if (available.contains(16)) 16 else available.minOption.getOrElse(16)

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
      stepSizePx = 16,
      pageBackgroundColorHex = PdfUtils.defaultPageBackgroundColor.toHex,
      printerMarginMm = 3.0
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
      val nextBase   = model.copy(buildConfigs = Some(list), selectedBuildConfigId = selectedId)
      val available  = nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize   = if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next       = nextBase.copy(stepSizePx = stepSize)
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.SetBuildConfig(id) =>
      val nextBase  = model.copy(selectedBuildConfigId = Some(id))
      val available = nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize  = if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next      = nextBase.copy(stepSizePx = stepSize)
      (next, Cmd.SideEffect(drawOverview(next)))
    case PrintInstructionsMsg.DrawOverview =>
      (model, Cmd.SideEffect(drawOverview(model)))
    case PrintInstructionsMsg.SetTitle(title) =>
      (model.copy(title = title), Cmd.None)
    case PrintInstructionsMsg.SetStepSize(px) =>
      (model.copy(stepSizePx = px), Cmd.None)
    case PrintInstructionsMsg.SetPageBackgroundColor(hex) =>
      (model.copy(pageBackgroundColorHex = hex), Cmd.None)
    case PrintInstructionsMsg.SetPrinterMarginMm(mm) =>
      (model.copy(printerMarginMm = mm), Cmd.None)
    case PrintInstructionsMsg.PrintPdf =>
      val pageBg = if (model.pageBackgroundColorHex.isBlank) PdfUtils.defaultPageBackgroundColor
                   else Color.fromHex(model.pageBackgroundColorHex)
      val request = PrintBookRequest(
        title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
        mosaicPicAndGridOpt = model.selectedStored.flatMap(stored =>
          mosaicPicAndGridForStored(stored, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))
        ),
        stepSizePx = model.stepSizePx,
        pageBackgroundColor = pageBg,
        printerMarginMm = model.printerMarginMm
      )
      (model, Cmd.SideEffect(PdfUtils.printBookPdf(request)))
    case PrintInstructionsMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val buildConfigs = model.buildConfigs.getOrElse(Nil)
    val selectedId   = model.selectedBuildConfigId.orElse(buildConfigs.headOption.map(_.id))
    val canPrint     = model.selectedStored.isDefined

    div(
      `class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container screen-container--narrow"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row", style := "gap: 0.5rem;")(
          GalleryLayout.backButton(PrintInstructionsMsg.Back, "Overview"),
          button(
            `class` := (if (canPrint) NesCss.btnPrimary else s"${NesCss.btn} btn-disabled"),
            onClick(PrintInstructionsMsg.PrintPdf)
          )(text("Print PDF"))
        ),
        None,
        false
      ),
      p(`class` := s"${NesCss.text} screen-intro")(
        text("Pick a mosaic setup, add a title, then create your PDF.")
      ),
      div(`class` := s"${NesCss.field} field-block")(
        label(`class` := "label-block")(text("Mosaic setup")),
        model.buildConfigs match {
          case None =>
            span(`class` := NesCss.text)(text("Loading…"))
          case Some(list) if list.isEmpty =>
            span(`class` := NesCss.text)(text("No setups saved yet. Create one from Mosaic setup."))
          case Some(list) =>
            select(
              `class` := s"${NesCss.input} input-min-w-16",
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
          div(`class` := "field-block--lg")(
            div(`class` := "section-title")(text("Layout preview")),
            div(onLoad(PrintInstructionsMsg.DrawOverview))(
              canvas(
                id := overviewCanvasId,
                width := 400,
                height := 200,
                `class` := "pixel-canvas"
              )()
            )
          )
        case None =>
          div()()
      },
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Title")),
        input(
          `type` := "text",
          `class` := s"${NesCss.input} input-w-full",
          value := model.title,
          onInput(PrintInstructionsMsg.SetTitle.apply)
        )
      ),
      stepSizeSliderBlock(model),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Page background color")),
        div(`class` := "flex-row flex-row--tight")(
          input(
            `type` := "color",
            `class` := "input-color",
            value := Color.normalizeHex(model.pageBackgroundColorHex, PdfUtils.defaultPageBackgroundColor.toHex),
            onInput(hex => PrintInstructionsMsg.SetPageBackgroundColor(hex))
          ),
          input(
            `type` := "text",
            `class` := s"${NesCss.input} input-w-7 input-monospace",
            value := model.pageBackgroundColorHex,
            placeholder := PdfUtils.defaultPageBackgroundColor.toHex,
            onInput(PrintInstructionsMsg.SetPageBackgroundColor.apply)
          )
        ),
        span(`class` := s"${NesCss.text} helper-text")(text("Color code (e.g. #fdfbe6) for all pages."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Margin (mm)")),
        input(
          `type` := "number",
          `class` := s"${NesCss.input} input-w-5",
          value := model.printerMarginMm.toString,
          min := "0",
          max := "20",
          step := "1",
          onInput(s => PrintInstructionsMsg.SetPrinterMarginMm(parsePrinterMargin(s)))
        ),
        span(`class` := s"${NesCss.text} helper-text--inline")(text("White border around each page. Default 3 mm."))
      )
    )
  }

  /** Step size: fixed values 12–32. Only options valid for selected build config are clickable; invalid ones shown greyed out. */
  private def stepSizeSliderBlock(model: Model): Html[Msg] = {
    val available = model.selectedStored
      .map(s => availableStepSizesForGrid(s.config.grid))
      .getOrElse(stepSizeCandidates)

    def pillClass(possible: Boolean, selected: Boolean): String = {
      val base = "step-size-pill"
      if (!possible) s"$base step-size-pill--disabled"
      else if (selected) s"$base step-size-pill--selected"
      else s"$base step-size-pill--unselected"
    }

    div(`class` := "step-size-block")(
      label(`class` := "label-block")(
        text("Section size (pixels)")
      ),
      div(`class` := "step-size-row step-size-row--wrap")(
        stepSizeCandidates.map { v =>
          val possible = available.contains(v)
          val selected = model.stepSizePx == v
          if (possible)
            button(
              `class` := pillClass(possible = true, selected = selected),
              onClick(PrintInstructionsMsg.SetStepSize(v))
            )(text(v.toString))
          else
            span(`class` := pillClass(possible = false, selected = false))(text(v.toString))
        }*
      ),
      span(`class` := "helper-text helper-text--top step-size-helper")(
        text("Only sizes that fit your layout evenly are available.")
      )
    )
  }


  /** Full image with palette applied (for overview canvas). */

  private def drawOverview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      model.selectedStored.flatMap(stored =>
        clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes).map(pic => (stored, pic))
      ) match {
        case Some((stored, pic)) =>
          CanvasUtils.drawFullImageWithGrid(canvas, ctx, pic, stored.config.grid, stored.config.offsetX, stored.config.offsetY, 400)
        case None =>
          CanvasUtils.drawPlaceholder(canvas, ctx, 400, 200, "Select a mosaic setup for preview")
      }
    })



  private def mosaicPicAndGridForStored(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): Option[(PixelPic, GridConfig)] =
    for {
      pic     <- clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes)
      cropped <- pic.crop(stored.config.offsetX, stored.config.offsetY, stored.config.grid.width, stored.config.grid.height)
    } yield (cropped, stored.config.grid)
}

private def parsePrinterMargin(s: String): Double = {
  val n = s.trim.toDoubleOption.getOrElse(3.0)
  n.max(0).min(20)
}

final case class PrintInstructionsModel(
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    selectedBuildConfigId: Option[String],
    title: String,
    stepSizePx: Int,
    pageBackgroundColorHex: String,
    printerMarginMm: Double
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
  case SetPageBackgroundColor(hex: String)
  case SetPrinterMarginMm(mm: Double)
  case DrawOverview
  case PrintPdf
  case Back
