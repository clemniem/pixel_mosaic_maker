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

import scala.concurrent.duration.DurationInt

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
  private val previewDebounceMs  = 250

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val base  = previous.collect { case ScreenOutput.OpenPrintConfig(stored) => stored }
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
      sideMarginMm = base.map(_.sideMarginMm).getOrElse(PdfUtils.defaultSideMarginMm),
      topBottomMarginMm = base.map(_.topBottomMarginMm).getOrElse(PdfUtils.defaultTopBottomMarginMm),
      contentTopOffsetMm = base.map(_.contentTopOffsetMm).getOrElse(PdfUtils.defaultContentTopOffsetMm),
      innerMargin = base.map(_.innerMargin).getOrElse(PdfUtils.defaultInnerMargin),
      pdfPreviewPageIdx = 0,
      savedConfigId = base.map(_.id),
      printConfigs = None,
      isPrintingPdf = false,
      previewRequestVersion = 0,
      previewCache = None,
      expandedInfoIds = Set.empty
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
      val selBuildId  = model.selectedBuildId.orElse(list.headOption.map(_.id))
      val selConfigId = selBuildId
        .flatMap(id => list.find(_.id == id).map(_.buildConfigRef))
        .orElse(model.selectedBuildConfigId)
      val next = invalidatePreview(
        model.copy(builds = Some(list), selectedBuildId = selBuildId, selectedBuildConfigId = selConfigId))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.LoadedBuildConfigs(list) =>
      // Only fall back to first config if no build has already set a config id
      val selectedId = model.selectedBuildConfigId.orElse(list.headOption.map(_.id))
      val nextBase   = model.copy(buildConfigs = Some(list), selectedBuildConfigId = selectedId)
      val available  =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = invalidatePreview(nextBase.copy(stepSizePx = stepSize))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.SetBuild(id) =>
      val configId = model.builds.getOrElse(Nil).find(_.id == id).map(_.buildConfigRef)
      val nextBase =
        model.copy(selectedBuildId = Some(id), selectedBuildConfigId = configId.orElse(model.selectedBuildConfigId))
      val available =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = invalidatePreview(nextBase.copy(stepSizePx = stepSize))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.LoadedImages(list) =>
      val next = invalidatePreview(model.copy(images = Some(list)))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.LoadedPalettes(list) =>
      val next = invalidatePreview(model.copy(palettes = Some(list)))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.SetBuildConfig(id) =>
      val nextBase  = model.copy(selectedBuildConfigId = Some(id))
      val available =
        nextBase.selectedStored.map(s => availableStepSizesForGrid(s.config.grid)).getOrElse(stepSizeCandidates)
      val stepSize =
        if (available.contains(nextBase.stepSizePx)) nextBase.stepSizePx else defaultStepSizeForAvailable(available)
      val next = invalidatePreview(nextBase.copy(stepSizePx = stepSize))
      (next, redrawOverviewAndRebuildPreviewCmd(next))
    case PrintInstructionsMsg.DrawOverview =>
      (model, drawOverviewCmd(model))
    case PrintInstructionsMsg.DrawPdfPreview =>
      (model, ensurePreviewIsReadyCmd(model))
    case PrintInstructionsMsg.SetTitle(title) =>
      val next = invalidatePreview(model.copy(title = title))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetStepSize(px) =>
      val next = invalidatePreview(model.copy(stepSizePx = px))
      (next, rebuildPreviewCmd(next))
    case PrintInstructionsMsg.SetPageBackgroundColor(hex) =>
      val next = invalidatePreview(model.copy(pageBackgroundColorHex = hex))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetSideMarginMm(mm) =>
      val next = invalidatePreview(model.copy(sideMarginMm = mm))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetTopBottomMarginMm(mm) =>
      val next = invalidatePreview(model.copy(topBottomMarginMm = mm))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetContentTopOffsetMm(mm) =>
      val next = invalidatePreview(model.copy(contentTopOffsetMm = mm))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetPatchBackgroundColor(hex) =>
      val next = invalidatePreview(model.copy(patchBackgroundColorHex = hex))
      (next, debouncePreviewCmd(next.previewRequestVersion))
    case PrintInstructionsMsg.SetStacked(value) =>
      val next = invalidatePreview(model.copy(stacked = value))
      (next, rebuildPreviewCmd(next))
    case PrintInstructionsMsg.SetInnerMargin(value) =>
      val next = invalidatePreview(model.copy(innerMargin = value))
      (next, rebuildPreviewCmd(next))
    case PrintInstructionsMsg.NextPdfPreviewPage =>
      val nextIdx = model.previewCache
        .filter(_.fingerprint == previewFingerprintForModel(model))
        .map(cache => clampPreviewPageIdx(model.pdfPreviewPageIdx + 1, cache.preview))
        .getOrElse(model.pdfPreviewPageIdx + 1)
      val next = model.copy(pdfPreviewPageIdx = nextIdx)
      (next, drawOrRebuildPreviewCmd(next))
    case PrintInstructionsMsg.PrevPdfPreviewPage =>
      val nextIdx = model.previewCache
        .filter(_.fingerprint == previewFingerprintForModel(model))
        .map(cache => clampPreviewPageIdx(model.pdfPreviewPageIdx - 1, cache.preview))
        .getOrElse((model.pdfPreviewPageIdx - 1).max(0))
      val next = model.copy(pdfPreviewPageIdx = nextIdx)
      (next, drawOrRebuildPreviewCmd(next))
    case PrintInstructionsMsg.DebouncedPreview(version) =>
      if (version == model.previewRequestVersion) (model, rebuildPreviewCmd(model))
      else (model, Cmd.None)
    case PrintInstructionsMsg.PreviewBuilt(version, fingerprint, preview) =>
      if (version != model.previewRequestVersion) (model, Cmd.None)
      else {
        val clamped = clampPreviewPageIdx(model.pdfPreviewPageIdx, preview)
        val next    = model.copy(
          pdfPreviewPageIdx = clamped,
          previewCache = Some(CachedPdfPreview(fingerprint, preview))
        )
        (next, drawPdfPreviewCmd(next))
      }
    case PrintInstructionsMsg.LoadedPrintConfigs(list) =>
      (model.copy(printConfigs = Some(list)), Cmd.None)
    case PrintInstructionsMsg.SaveConfig =>
      val id     = model.savedConfigId.getOrElse(LocalStorageUtils.newId("printconfig"))
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
        sideMarginMm = model.sideMarginMm,
        topBottomMarginMm = model.topBottomMarginMm,
        contentTopOffsetMm = model.contentTopOffsetMm,
        innerMargin = model.innerMargin
      )
      val existing = model.printConfigs.getOrElse(Nil)
      val updated  = stored :: existing.filterNot(_.id == id)
      val saveCmd  = LocalStorageUtils.saveList(StorageKeys.printConfigs, updated)(
        list => PrintInstructionsMsg.ConfigSaved(id, list),
        (_, _) => PrintInstructionsMsg.ConfigSaved(id, updated)
      )
      (model, saveCmd)
    case PrintInstructionsMsg.ConfigSaved(id, newList) =>
      (model.copy(savedConfigId = Some(id), printConfigs = Some(newList)), Cmd.None)
    case PrintInstructionsMsg.PrintPdf =>
      if (!model.selectedStored.isDefined || model.isPrintingPdf) (model, Cmd.None)
      else {
        val next = model.copy(isPrintingPdf = true)
        (
          next,
          CmdUtils.run(
            CanvasUtils.runAfterNextFrame(PdfUtils.printBookPdf(bookRequestForModel(next))),
            _ => PrintInstructionsMsg.PrintPdfFinished,
            _ => PrintInstructionsMsg.PrintPdfFailed
          )
        )
      }
    case PrintInstructionsMsg.PrintPdfFinished =>
      (model.copy(isPrintingPdf = false), Cmd.None)
    case PrintInstructionsMsg.PrintPdfFailed =>
      (model.copy(isPrintingPdf = false), Cmd.None)
    case PrintInstructionsMsg.Back =>
      (model, navCmd(ScreenId.PrintConfigsId, None))
    case PrintInstructionsMsg.ToggleInfo(id) =>
      val next =
        if (model.expandedInfoIds.contains(id)) model.expandedInfoIds - id
        else model.expandedInfoIds + id
      (model.copy(expandedInfoIds = next), Cmd.None)
    case PrintInstructionsMsg.NoOp =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val canPrint  = model.selectedStored.isDefined
    val printBusy = model.isPrintingPdf

    div(
      `class` := s"${NesCss.screenContainer} screen-container--narrow"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row", style := "gap: 0.5rem;")(
          GalleryLayout.backButton(PrintInstructionsMsg.Back, "Print"),
          button(`class` := NesCss.btn, onClick(PrintInstructionsMsg.SaveConfig))(text("Save")),
          button(
            `class` := (if (canPrint && !printBusy) NesCss.btnPrimary else s"${NesCss.btn} btn-disabled"),
            onClick(if (canPrint && !printBusy) PrintInstructionsMsg.PrintPdf else PrintInstructionsMsg.NoOp)
          )(text(if (printBusy) "Preparing PDF..." else "Print PDF")),
          if (printBusy)
            span(`class` := s"${NesCss.text} helper-text--inline")(text("Please wait while the PDF is generated."))
          else
            span()()
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
        infoLabel("Page background color", "page-bg", "Color code (e.g. #fdfbe6) for all pages.", model),
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
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        infoLabel("Step background color", "patch-bg", "Background for non-active pixels in layer steps.", model),
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
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg build-stacked-block")(
        infoLabel("Stacked layers", "stacked", "On = cumulative colors per layer. Off = one color per layer.", model),
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
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        infoLabel("Side margin (mm)", "side-margin", "Left/right white border. Default 6 mm.", model),
        input(
          `type`  := "number",
          `class` := s"${NesCss.input} input-w-6",
          value   := model.sideMarginMm.toString,
          min     := "0",
          max     := "20",
          step    := "0.1",
          onInput(s => PrintInstructionsMsg.SetSideMarginMm(parseMarginMm(s)))
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        infoLabel("Top/bottom margin (mm)", "tb-margin", "Top/bottom white border. Default 5 mm.", model),
        input(
          `type`  := "number",
          `class` := s"${NesCss.input} input-w-6",
          value   := model.topBottomMarginMm.toString,
          min     := "0",
          max     := "20",
          step    := "0.1",
          onInput(s => PrintInstructionsMsg.SetTopBottomMarginMm(parseMarginMm(s)))
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg build-stacked-block")(
        infoLabel(
          "Inner page margin",
          "inner-margin",
          "Off = no white gap where pages meet. On = uniform margins.",
          model),
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
        )
      ),
      div(`class` := s"${NesCss.field} field-block--lg")(
        div(`class` := "label-with-info")(
          label(`class` := "label-block")(
            span(`class` := NesCss.text)(text("Content top offset (mm):")),
            span(`class` := "offset-value")(text(f"${model.contentTopOffsetMm}%.1f"))
          ),
          span(`class` := "info-toggle", onClick(PrintInstructionsMsg.ToggleInfo("top-offset")))(text("i"))
        ),
        span(
          `class` := (if (model.expandedInfoIds.contains("top-offset")) "info-hint info-hint--visible nes-text"
                      else "info-hint nes-text"))(
          text("Push all page content down. Default 2 mm.")
        ),
        input(
          `type` := "range",
          min    := "0",
          max    := "40",
          step   := "0.5",
          value  := model.contentTopOffsetMm.toString,
          onInput(s =>
            PrintInstructionsMsg.SetContentTopOffsetMm(s.toDoubleOption.getOrElse(PdfUtils.defaultContentTopOffsetMm)))
        )
      )
    )
  }

  private def infoLabel(labelText: String, infoId: String, hintText: String, model: Model): Html[Msg] = {
    val expanded  = model.expandedInfoIds.contains(infoId)
    val hintClass = if (expanded) "info-hint info-hint--visible nes-text" else "info-hint nes-text"
    div(
      div(`class` := "label-with-info")(
        label(`class` := "label-block")(text(labelText)),
        span(`class` := "info-toggle", onClick(PrintInstructionsMsg.ToggleInfo(infoId)))(text("i"))
      ),
      span(`class` := hintClass)(text(hintText))
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

    val stepInfoExpanded = model.expandedInfoIds.contains("step-size")
    val stepHintClass    =
      if (stepInfoExpanded) "info-hint info-hint--visible nes-text helper-text--top"
      else "info-hint nes-text helper-text--top"
    div(`class` := "step-size-block")(
      div(`class` := "label-with-info")(
        label(`class` := "label-block")(text("Section size (pixels)")),
        span(`class` := "info-toggle", onClick(PrintInstructionsMsg.ToggleInfo("step-size")))(text("i"))
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
      span(`class` := stepHintClass)(
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
        model.previewCache match {
          case Some(cache) if cache.fingerprint == previewFingerprintForModel(model) =>
            val preview = cache.preview
            val idx     = clampPreviewPageIdx(model.pdfPreviewPageIdx, preview)
            val pageBg  =
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
                preview.sideMarginMm,
                preview.topBottomMarginMm,
                preview.removeInnerMargin,
                pageIndex0Based = idx,
                preview.totalPages
              )
          case _ =>
            CanvasUtils.drawPlaceholder(canvas, ctx, canvas.width, canvas.height, "Preparing preview...")
        }
      }
    }

  private def drawOverviewCmd(model: Model): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(drawOverview(model), PrintInstructionsMsg.NoOp, _ => PrintInstructionsMsg.NoOp)

  private def drawPdfPreviewCmd(model: Model): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(drawPdfPreview(model), PrintInstructionsMsg.NoOp, _ => PrintInstructionsMsg.NoOp)

  private def redrawOverviewAndRebuildPreviewCmd(model: Model): Cmd[IO, Msg] =
    Cmd.Batch(drawOverviewCmd(model), rebuildPreviewCmd(model))

  private def drawOrRebuildPreviewCmd(model: Model): Cmd[IO, Msg] =
    if (hasUsablePreview(model)) drawPdfPreviewCmd(model) else rebuildPreviewCmd(model)

  private def ensurePreviewIsReadyCmd(model: Model): Cmd[IO, Msg] =
    if (model.selectedStored.isEmpty || hasUsablePreview(model)) drawPdfPreviewCmd(model) else rebuildPreviewCmd(model)

  private def debouncePreviewCmd(version: Int): Cmd[IO, Msg] =
    CmdUtils.run(
      IO.sleep(previewDebounceMs.millis).as(version),
      PrintInstructionsMsg.DebouncedPreview.apply,
      _ => PrintInstructionsMsg.NoOp
    )

  private def rebuildPreviewCmd(model: Model): Cmd[IO, Msg] =
    if (model.selectedStored.isEmpty) drawPdfPreviewCmd(model)
    else
      CmdUtils.run(
        buildPreview(model, model.previewRequestVersion),
        { case (version, fingerprint, preview) => PrintInstructionsMsg.PreviewBuilt(version, fingerprint, preview) },
        _ => PrintInstructionsMsg.NoOp
      )

  private def buildPreview(model: Model, version: Int): IO[(Int, PreviewFingerprint, PdfUtils.BookPreview)] = IO {
    val fingerprint = previewFingerprintForModel(model)
    val preview     = PdfUtils.previewBookPages(bookRequestForModel(model))
    (version, fingerprint, preview)
  }

  private def hasUsablePreview(model: Model): Boolean =
    model.previewCache.exists(_.fingerprint == previewFingerprintForModel(model))

  private def clampPreviewPageIdx(idx: Int, preview: PdfUtils.BookPreview): Int =
    if (preview.pages.isEmpty) 0 else idx.max(0).min(preview.pages.size - 1)

  private def invalidatePreview(model: Model): Model =
    model.copy(previewRequestVersion = model.previewRequestVersion + 1, previewCache = None)

  private def previewFingerprintForModel(model: Model): PreviewFingerprint =
    PreviewFingerprint(
      selectedBuildConfigId = model.selectedBuildConfigId,
      title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
      stepSizePx = model.stepSizePx,
      pageBackgroundColorHex =
        Color.normalizeHex(model.pageBackgroundColorHex, PdfUtils.defaultPageBackgroundColor.toHex),
      patchBackgroundColorHex = Color.normalizeHex(model.patchBackgroundColorHex, Color.layerPatchBackground.toHex),
      stacked = model.stacked,
      sideMarginMm = model.sideMarginMm,
      topBottomMarginMm = model.topBottomMarginMm,
      contentTopOffsetMm = model.contentTopOffsetMm,
      innerMargin = model.innerMargin
    )

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
      sideMarginMm = model.sideMarginMm,
      topBottomMarginMm = model.topBottomMarginMm,
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
      pic     <- clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes)
      cropped <- pic.crop(
        stored.config.offsetX,
        stored.config.offsetY,
        stored.config.grid.width,
        stored.config.grid.height)
    } yield (cropped, stored.config.grid)
}

private def parseMarginMm(s: String): Double = {
  val n = s.trim.toDoubleOption.getOrElse(0.0)
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
  sideMarginMm: Double,
  topBottomMarginMm: Double,
  contentTopOffsetMm: Double,
  innerMargin: Boolean,
  pdfPreviewPageIdx: Int,
  savedConfigId: Option[String],
  printConfigs: Option[List[StoredPrintConfig]],
  isPrintingPdf: Boolean,
  previewRequestVersion: Int,
  previewCache: Option[CachedPdfPreview],
  expandedInfoIds: Set[String]) {
  def selectedStored: Option[StoredBuildConfig] =
    buildConfigs.flatMap(list => selectedBuildConfigId.flatMap(id => list.find(_.id == id)))
}

final case class PreviewFingerprint(
  selectedBuildConfigId: Option[String],
  title: String,
  stepSizePx: Int,
  pageBackgroundColorHex: String,
  patchBackgroundColorHex: String,
  stacked: Boolean,
  sideMarginMm: Double,
  topBottomMarginMm: Double,
  contentTopOffsetMm: Double,
  innerMargin: Boolean)

final case class CachedPdfPreview(
  fingerprint: PreviewFingerprint,
  preview: PdfUtils.BookPreview)

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
  case SetSideMarginMm(mm: Double)
  case SetTopBottomMarginMm(mm: Double)
  case SetContentTopOffsetMm(mm: Double)
  case SetPatchBackgroundColor(hex: String)
  case SetStacked(value: Boolean)
  case SetInnerMargin(value: Boolean)
  case DrawOverview
  case DrawPdfPreview
  case NextPdfPreviewPage
  case PrevPdfPreviewPage
  case DebouncedPreview(version: Int)
  case PreviewBuilt(version: Int, fingerprint: PreviewFingerprint, preview: PdfUtils.BookPreview)
  case SaveConfig
  case ConfigSaved(id: String, newList: List[StoredPrintConfig])
  case PrintPdf
  case PrintPdfFinished
  case PrintPdfFailed
  case ToggleInfo(id: String)
  case Back
  case NoOp
}
