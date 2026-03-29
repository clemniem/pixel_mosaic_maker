package clemniem.screens

import cats.effect.IO
import clemniem.{
  Color,
  Layout,
  PixelPic,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuild,
  StoredBuildConfig,
  StoredImage,
  StoredPalette,
  StoredPrintConfig
}
import clemniem.common.{CanvasUtils, CmdUtils, LocalStorageUtils, PdfUtils, PixelPicCanvas, PrintBookRequest}
import clemniem.common.pdf.PdfPreviewRenderer
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*


/** Fixed step size options (px). Only those that divide every section width/height are shown. Default 16 if available
  * else smallest.
  */
private val stepSizeCandidates = List(12, 16, 20, 24, 28, 32)

private def availableStepSizesForGrid(grid: Layout): List[Int] =
  stepSizeCandidates.filter(s => grid.parts.forall(p => p.width % s == 0 && p.height % s == 0))

private def defaultStepSizeForAvailable(available: List[Int]): Int =
  if (available.contains(16)) 16 else available.minOption.getOrElse(16)

/** Screen to generate the PDF print instructions: choose a BuildConfig and set options (e.g. title). */
object PrintInstructionsScreen extends Screen {
  type Model = PrintInstructionsModel
  type Msg   = PrintInstructionsMsg

  val screenId: ScreenId         = ScreenId.PrintInstructionsId
  private val overviewCanvasId   = "print-instructions-overview"
  private val pdfPreviewCanvasId = "print-instructions-pdf-preview"
  private val pdfPreviewMaxPages = 5

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val base = previous.collect { case ScreenOutput.OpenPrintConfig(stored) => stored }
    val model = PrintInstructionsModel(
      builds = None,
      buildConfigs = None,
      images = None,
      palettes = None,
      selectedBuildId = base.flatMap(_.selectedBuildId),
      selectedBuildConfigId = base.flatMap(_.selectedBuildConfigId),
      title = base.map(_.title).getOrElse(PdfUtils.defaultTitle),
      stepSizePx = base.map(_.stepSizePx).getOrElse(PdfUtils.defaultStepSizePx),
      pageBackgroundColorHex = base.map(_.pageBackgroundColorHex).getOrElse(PdfUtils.defaultPageBackgroundColor.toHex),
      patchBackgroundColorHex = base.map(_.patchBackgroundColorHex).getOrElse(Color.layerPatchBackground.toHex),
      stacked = base.map(_.stacked).getOrElse(PdfUtils.defaultStacked),
      printerMarginMm = base.map(_.printerMarginMm).getOrElse(PdfUtils.defaultPrinterMarginMm),
      contentTopOffsetMm = base.map(_.contentTopOffsetMm).getOrElse(PdfUtils.defaultContentTopOffsetMm),
      innerMargin = base.map(_.innerMargin).getOrElse(PdfUtils.defaultInnerMargin),
      pdfPreviewPageIdx = 0,
      savedConfigId = base.map(_.id),
      printConfigs = None
    )
    val loadBuilds = LocalStorageUtils.loadList(StorageKeys.builds)(
      PrintInstructionsMsg.LoadedBuilds.apply,
      (_, _) => PrintInstructionsMsg.LoadedBuilds(Nil)
    )
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      PrintInstructionsMsg.LoadedBuildConfigs.apply,
      (_, _) => PrintInstructionsMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      PrintInstructionsMsg.LoadedImages.apply,
      (_, _) => PrintInstructionsMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      PrintInstructionsMsg.LoadedPalettes.apply,
      (_, _) => PrintInstructionsMsg.LoadedPalettes(Nil)
    )
    val loadPrintConfigs = LocalStorageUtils.loadList(StorageKeys.printConfigs)(
      PrintInstructionsMsg.LoadedPrintConfigs.apply,
      (_, _) => PrintInstructionsMsg.LoadedPrintConfigs(Nil)
    )
    (model, Cmd.Batch(loadBuilds, loadBuildConfigs, loadImages, loadPalettes, loadPrintConfigs))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PrintInstructionsMsg.LoadedBuilds(list) =>
      // Auto-select first build (if nothing already selected), and derive build config from it
      val selBuildId = model.selectedBuildId.orElse(list.headOption.map(_.id))
      val selConfigId = selBuildId
        .flatMap(id => list.find(_.id == id).map(_.buildConfigRef))
        .orElse(model.selectedBuildConfigId)
      val next = model.copy(builds = Some(list), selectedBuildId = selBuildId, selectedBuildConfigId = selConfigId)
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.LoadedBuildConfigs(list) =>
      // Only fall back to first config if no build has already set a config id
      val selectedId = model.selectedBuildConfigId.orElse(list.headOption.map(_.id))
      val nextBase   = model.copy(buildConfigs = Some(list), selectedBuildConfigId = selectedId)
      val available =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = nextBase.copy(stepSizePx = stepSize)
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.SetBuild(id) =>
      val configId = model.builds.getOrElse(Nil).find(_.id == id).map(_.buildConfigRef)
      val nextBase = model.copy(selectedBuildId = Some(id), selectedBuildConfigId = configId.orElse(model.selectedBuildConfigId))
      val available =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = nextBase.copy(stepSizePx = stepSize)
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.SetBuildConfig(id) =>
      val nextBase = model.copy(selectedBuildConfigId = Some(id))
      val available =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = nextBase.copy(stepSizePx = stepSize)
      (next, drawPreviewsCmd(next))
    case PrintInstructionsMsg.DrawOverview =>
      (model, drawPreviewsCmd(model))
    case PrintInstructionsMsg.DrawPdfPreview =>
      (model, drawPdfPreviewCmd(model))
    case PrintInstructionsMsg.SetTitle(title) =>
      val next = model.copy(title = title)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetStepSize(px) =>
      val next = model.copy(stepSizePx = px)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetPageBackgroundColor(hex) =>
      val next = model.copy(pageBackgroundColorHex = hex)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetPrinterMarginMm(mm) =>
      val next = model.copy(printerMarginMm = mm)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetContentTopOffsetMm(mm) =>
      val next = model.copy(contentTopOffsetMm = mm)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetPatchBackgroundColor(hex) =>
      val next = model.copy(patchBackgroundColorHex = hex)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetStacked(value) =>
      val next = model.copy(stacked = value)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.SetInnerMargin(value) =>
      val next = model.copy(innerMargin = value)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.NextPdfPreviewPage =>
      val next = model.copy(pdfPreviewPageIdx = model.pdfPreviewPageIdx + 1)
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.PrevPdfPreviewPage =>
      val next = model.copy(pdfPreviewPageIdx = (model.pdfPreviewPageIdx - 1).max(0))
      (next, drawPdfPreviewCmd(next))
    case PrintInstructionsMsg.LoadedPrintConfigs(list) =>
      (model.copy(printConfigs = Some(list)), Cmd.None)
    case PrintInstructionsMsg.SaveConfig =>
      val id = model.savedConfigId.getOrElse(LocalStorageUtils.newId("printconfig"))
      val stored = StoredPrintConfig(
        id = id,
        name = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
        selectedBuildId = model.selectedBuildId,
        selectedBuildConfigId = model.selectedBuildConfigId,
        title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
        stepSizePx = model.stepSizePx,
        pageBackgroundColorHex = model.pageBackgroundColorHex,
        patchBackgroundColorHex = model.patchBackgroundColorHex,
        stacked = model.stacked,
        printerMarginMm = model.printerMarginMm,
        contentTopOffsetMm = model.contentTopOffsetMm,
        innerMargin = model.innerMargin
      )
      val existing = model.printConfigs.getOrElse(Nil)
      val updated  = stored :: existing.filterNot(_.id == id)
      val saveCmd = LocalStorageUtils.saveList(StorageKeys.printConfigs, updated)(
        list => PrintInstructionsMsg.ConfigSaved(id, list),
        (_, _) => PrintInstructionsMsg.ConfigSaved(id, updated)
      )
      (model, saveCmd)
    case PrintInstructionsMsg.ConfigSaved(id, newList) =>
      (model.copy(savedConfigId = Some(id), printConfigs = Some(newList)), Cmd.None)
    case PrintInstructionsMsg.PrintPdf =>
      (model, CmdUtils.fireAndForget(PdfUtils.printBookPdf(bookRequestForModel(model)), PrintInstructionsMsg.NoOp, _ => PrintInstructionsMsg.NoOp))
    case PrintInstructionsMsg.Back =>
      (model, navCmd(ScreenId.PrintConfigsId, None))
    case PrintInstructionsMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val canPrint = model.selectedStored.isDefined

    div(
      `class` := s"${NesCss.screenContainer} screen-container--narrow"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row", style := "gap: 0.5rem;")(
          GalleryLayout.backButton(PrintInstructionsMsg.Back, "Print"),
          button(`class` := NesCss.btn, onClick(PrintInstructionsMsg.SaveConfig))(text("Save")),
          button(
            `class` := (if (canPrint) NesCss.btnPrimary else s"${NesCss.btn} btn-disabled"),
            onClick(PrintInstructionsMsg.PrintPdf)
          )(text("Print PDF"))
        ),
        None,
        false
      ),
      p(`class` := s"${NesCss.text} screen-intro")(
        text("Pick a build, add a title, then create your PDF.")
      ),
      buildSelectorBlock(model),
      model.buildConfigs match {
        case Some(_) =>
          div(`class` := "field-block--lg")(
            div(`class` := "print-preview-row")(
              div(`class` := "print-preview-col")(
                div(`class` := "section-title")(text("Layout preview")),
                div(onLoad(PrintInstructionsMsg.DrawOverview))(
                  canvas(
                    id      := overviewCanvasId,
                    width   := 400,
                    height  := 200,
                    `class` := "pixel-canvas"
                  )()
                )
              ),
              div(`class` := "print-preview-col")(
                div(`class` := "section-title")(
                  text("PDF preview")
                ),
                div(onLoad(PrintInstructionsMsg.DrawPdfPreview))(
                  canvas(
                    id      := pdfPreviewCanvasId,
                    width   := 260,
                    height  := 260,
                    `class` := "pixel-canvas print-pdf-preview-canvas",
                    onClick(PrintInstructionsMsg.NextPdfPreviewPage),
                    title := "Click to go to next page"
                  )()
                ),
                div(`class` := "flex-row flex-row--tight", style := "margin-top: 0.3rem; justify-content: center;")(
                  button(`class` := NesCss.btn, onClick(PrintInstructionsMsg.PrevPdfPreviewPage))(
                    GalleryLayout.backButtonLabel("←", "Prev")
                  ),
                  button(`class` := NesCss.btn, onClick(PrintInstructionsMsg.NextPdfPreviewPage))(
                    GalleryLayout.nextButtonLabel("Next", "→")
                  )
                )
              )
            )
          )
        case None =>
          div()()
      },
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Title")),
        input(
          `type`  := "text",
          `class` := s"${NesCss.input} input-w-full",
          value   := model.title,
          onInput(PrintInstructionsMsg.SetTitle.apply)
        )
      ),
      stepSizeSliderBlock(model),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Page background color")),
        div(`class` := "flex-row flex-row--tight")(
          input(
            `type`  := "color",
            `class` := "input-color",
            value   := Color.normalizeHex(model.pageBackgroundColorHex, PdfUtils.defaultPageBackgroundColor.toHex),
            onInput(hex => PrintInstructionsMsg.SetPageBackgroundColor(hex))
          ),
          input(
            `type`      := "text",
            `class`     := s"${NesCss.input} input-w-7 input-monospace",
            value       := model.pageBackgroundColorHex,
            placeholder := PdfUtils.defaultPageBackgroundColor.toHex,
            onInput(PrintInstructionsMsg.SetPageBackgroundColor.apply)
          )
        ),
        span(`class` := s"${NesCss.text} helper-text")(text("Color code (e.g. #fdfbe6) for all pages."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Step background color")),
        div(`class` := "flex-row flex-row--tight")(
          input(
            `type`  := "color",
            `class` := "input-color",
            value   := Color.normalizeHex(model.patchBackgroundColorHex, Color.layerPatchBackground.toHex),
            onInput(hex => PrintInstructionsMsg.SetPatchBackgroundColor(hex))
          ),
          input(
            `type`      := "text",
            `class`     := s"${NesCss.input} input-w-7 input-monospace",
            value       := model.patchBackgroundColorHex,
            placeholder := Color.layerPatchBackground.toHex,
            onInput(PrintInstructionsMsg.SetPatchBackgroundColor.apply)
          )
        ),
        span(`class` := s"${NesCss.text} helper-text")(text("Background for non-active pixels in layer steps."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg build-stacked-block")(
        label(`class` := "label-block")(text("Stacked layers")),
        div(`class` := "stacked-radios")(
          label(`class` := "stacked-radio-option")(
            input(
              `type`  := "radio",
              `class` := NesCss.radio,
              name    := "pdf-stacked",
              value   := "on",
              checked := model.stacked,
              onClick(PrintInstructionsMsg.SetStacked(true))
            ),
            span(text("On"))
          ),
          label(`class` := "stacked-radio-option")(
            input(
              `type`  := "radio",
              `class` := NesCss.radio,
              name    := "pdf-stacked",
              value   := "off",
              checked := !model.stacked,
              onClick(PrintInstructionsMsg.SetStacked(false))
            ),
            span(text("Off"))
          )
        ),
        span(`class` := s"${NesCss.text} helper-text")(text("On = cumulative colors per layer. Off = one color per layer."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Margin (mm)")),
        input(
          `type`  := "number",
          `class` := s"${NesCss.input} input-w-5",
          value   := model.printerMarginMm.toString,
          min     := "0",
          max     := "20",
          step    := "1",
          onInput(s => PrintInstructionsMsg.SetPrinterMarginMm(parsePrinterMargin(s)))
        ),
        span(`class` := s"${NesCss.text} helper-text--inline")(text("White border around each page. Default 6 mm."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg build-stacked-block")(
        label(`class` := "label-block")(text("Inner page margin")),
        div(`class` := "stacked-radios")(
          label(`class` := "stacked-radio-option")(
            input(
              `type`  := "radio",
              `class` := NesCss.radio,
              name    := "pdf-inner-margin",
              value   := "on",
              checked := model.innerMargin,
              onClick(PrintInstructionsMsg.SetInnerMargin(true))
            ),
            span(text("On"))
          ),
          label(`class` := "stacked-radio-option")(
            input(
              `type`  := "radio",
              `class` := NesCss.radio,
              name    := "pdf-inner-margin",
              value   := "off",
              checked := !model.innerMargin,
              onClick(PrintInstructionsMsg.SetInnerMargin(false))
            ),
            span(text("Off"))
          )
        ),
        span(`class` := s"${NesCss.text} helper-text")(text("Off = no white gap where pages meet. On = uniform margins."))
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(
          span(`class` := NesCss.text)(text("Content top offset (mm):")),
          span(`class` := "offset-value")(text(f"${model.contentTopOffsetMm}%.1f"))
        ),
        input(
          `type` := "range",
          min    := "0",
          max    := "40",
          step   := "0.5",
          value  := model.contentTopOffsetMm.toString,
          onInput(s => PrintInstructionsMsg.SetContentTopOffsetMm(s.toDoubleOption.getOrElse(PdfUtils.defaultContentTopOffsetMm)))
        ),
        span(`class` := s"${NesCss.text} helper-text--inline")(text("Push all page content down. Default 2 mm."))
      )
    )
  }

  private def buildSelectorBlock(model: Model): Html[Msg] = {
    val builds = model.builds.getOrElse(Nil)
    if (builds.nonEmpty) {
      val selectedId = model.selectedBuildId.orElse(builds.headOption.map(_.id))
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Mosaic build")),
        tyrian.Html.select(
          `class` := s"${NesCss.input} input-w-full",
          onChange(PrintInstructionsMsg.SetBuild.apply)
        )(
          builds.map { b =>
            val attrs =
              if (selectedId.contains(b.id)) List(Attribute("value", b.id), selected(true))
              else List(Attribute("value", b.id))
            option(attrs*)(text(b.name))
          }*
        )
      )
    } else {
      val buildConfigs = model.buildConfigs.getOrElse(Nil)
      val selectedId   = model.selectedBuildConfigId.orElse(buildConfigs.headOption.map(_.id))
      div(`class` := s"${NesCss.field} field-block--lg")(
        label(`class` := "label-block")(text("Build configuration")),
        tyrian.Html.select(
          `class` := s"${NesCss.input} input-w-full",
          onChange(PrintInstructionsMsg.SetBuildConfig.apply)
        )(
          buildConfigs.map { bc =>
            val attrs =
              if (selectedId.contains(bc.id)) List(Attribute("value", bc.id), selected(true))
              else List(Attribute("value", bc.id))
            option(attrs*)(text(bc.name))
          }*
        )
      )
    }
  }

  /** Step size: fixed values 12–32. Only options valid for selected build config are clickable; invalid ones shown
    * greyed out.
    */
  private def stepSizeSliderBlock(model: Model): Html[Msg] = {
    val available = model.selectedStored
      .map(s => availableStepSizesForGrid(s.config.grid))
      .getOrElse(stepSizeCandidates)

    def pillClass(possible: Boolean, selected: Boolean): String = {
      val extra = "step-size-pill"
      if (!possible) s"pill $extra pill--disabled"
      else if (selected) s"pill $extra pill--selected"
      else s"pill $extra pill--unselected"
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
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1) { (canvas, ctx) =>
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      model.selectedStored.flatMap(stored =>
        clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes).map(pic => (stored, pic))) match {
        case Some((stored, pic)) =>
          PixelPicCanvas.drawFullImageWithGrid(
            canvas,
            ctx,
            pic,
            stored.config.grid,
            stored.config.offsetX,
            stored.config.offsetY,
            400)
        case None =>
          CanvasUtils.drawPlaceholder(canvas, ctx, 400, 200, "Select a mosaic setup for preview")
      }
    }

  private def drawPdfPreview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(pdfPreviewCanvasId, maxRetries = 100, delayMs = 1) { (canvas, ctx) =>
      if (model.selectedStored.isEmpty) {
        CanvasUtils.drawPlaceholder(canvas, ctx, canvas.width, canvas.height, "Select a mosaic setup for preview")
      } else {
        val request = bookRequestForModel(model)
        val preview = PdfUtils.previewBookPages(request, pdfPreviewMaxPages)
        val idx     = if (preview.pages.isEmpty) 0 else model.pdfPreviewPageIdx.max(0).min(preview.pages.size - 1)
        val pageBg =
          if (model.pageBackgroundColorHex.isBlank) PdfUtils.defaultPageBackgroundColor
          else Color.fromHex(model.pageBackgroundColorHex)
        if (preview.pages.isEmpty)
          CanvasUtils.drawPlaceholder(canvas, ctx, canvas.width, canvas.height, "Preview unavailable")
        else
          PdfPreviewRenderer.render(
            canvas,
            preview.pages(idx),
            preview.pageWmm,
            preview.pageHmm,
            pageBg.r,
            pageBg.g,
            pageBg.b,
            preview.printerMarginMm,
            preview.removeInnerMargin,
            pageIndex0Based = idx,
            preview.totalPages
          )
      }
    }

  private def drawPreviewsCmd(model: Model): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(drawPreviews(model), PrintInstructionsMsg.NoOp, _ => PrintInstructionsMsg.NoOp)

  private def drawPdfPreviewCmd(model: Model): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(drawPdfPreview(model), PrintInstructionsMsg.NoOp, _ => PrintInstructionsMsg.NoOp)

  private def drawPreviews(model: Model): IO[Unit] =
    drawOverview(model).flatMap(_ => drawPdfPreview(model))

  private def bookRequestForModel(model: Model): PrintBookRequest = {
    val pageBg =
      if (model.pageBackgroundColorHex.isBlank) PdfUtils.defaultPageBackgroundColor
      else Color.fromHex(model.pageBackgroundColorHex)
    val patchBg =
      if (model.patchBackgroundColorHex.isBlank) Color.layerPatchBackground
      else Color.fromHex(model.patchBackgroundColorHex)
    PrintBookRequest(
      title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
      mosaicPicAndGridOpt = model.selectedStored.flatMap(stored =>
        mosaicPicAndGridForStored(stored, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))),
      stepSizePx = model.stepSizePx,
      pageBackgroundColor = pageBg,
      printerMarginMm = model.printerMarginMm,
      contentTopOffsetMm = model.contentTopOffsetMm,
      patchBackgroundColor = patchBg,
      stacked = model.stacked,
      innerMargin = model.innerMargin
    )
  }

  private def mosaicPicAndGridForStored(
    stored: StoredBuildConfig,
    images: List[StoredImage],
    palettes: List[StoredPalette]
  ): Option[(PixelPic, Layout)] =
    for {
      pic <- clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes)
      cropped <- pic.crop(
        stored.config.offsetX,
        stored.config.offsetY,
        stored.config.grid.width,
        stored.config.grid.height)
    } yield (cropped, stored.config.grid)
}

private def parsePrinterMargin(s: String): Double = {
  val n = s.trim.toDoubleOption.getOrElse(3.0)
  n.max(0).min(20)
}

final case class PrintInstructionsModel(
  builds: Option[List[StoredBuild]],
  buildConfigs: Option[List[StoredBuildConfig]],
  images: Option[List[StoredImage]],
  palettes: Option[List[StoredPalette]],
  selectedBuildId: Option[String],
  selectedBuildConfigId: Option[String],
  title: String,
  stepSizePx: Int,
  pageBackgroundColorHex: String,
  patchBackgroundColorHex: String,
  stacked: Boolean,
  printerMarginMm: Double,
  contentTopOffsetMm: Double,
  innerMargin: Boolean,
  pdfPreviewPageIdx: Int,
  savedConfigId: Option[String],
  printConfigs: Option[List[StoredPrintConfig]]) {
  def selectedStored: Option[StoredBuildConfig] =
    buildConfigs.flatMap(list => selectedBuildConfigId.flatMap(id => list.find(_.id == id)))
}

enum PrintInstructionsMsg {
  case LoadedBuilds(list: List[StoredBuild])
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case LoadedPrintConfigs(list: List[StoredPrintConfig])
  case SetBuild(id: String)
  case SetBuildConfig(id: String)
  case SetTitle(title: String)
  case SetStepSize(px: Int)
  case SetPageBackgroundColor(hex: String)
  case SetPrinterMarginMm(mm: Double)
  case SetContentTopOffsetMm(mm: Double)
  case SetPatchBackgroundColor(hex: String)
  case SetStacked(value: Boolean)
  case SetInnerMargin(value: Boolean)
  case DrawOverview
  case DrawPdfPreview
  case NextPdfPreviewPage
  case PrevPdfPreviewPage
  case SaveConfig
  case ConfigSaved(id: String, newList: List[StoredPrintConfig])
  case PrintPdf
  case Back
  case NoOp
}
