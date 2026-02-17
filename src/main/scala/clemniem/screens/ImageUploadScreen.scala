package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.{
  AutoQuantize,
  Color,
  FromPalette,
  NavigateNext,
  PaletteMode,
  PixelPic,
  PixelPicService,
  Screen,
  ScreenId,
  StorageKeys,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, CmdUtils, ImageUtils, LocalStorageUtils}
import clemniem.common.image.{
  ColorDithering,
  ColorQuantizationService,
  DownscaleAverage,
  DownscaleBayer,
  DownscaleStrategy,
  FloydSteinbergDithering,
  NoColorDithering,
  OrderedBayerDithering,
  SizeReductionService
}
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js
import scala.concurrent.duration.DurationInt

/** Upload image: max 5000×5000, auto downscale to 500×500 with optional dithering, optional palette 4–16 colors. */
object ImageUploadScreen extends Screen {
  type Model = ImageUploadModel
  type Msg   = ImageUploadMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ImageUploadId

  private val targetMaxWidth  = SizeReductionService.TargetMaxWidth
  private val targetMaxHeight = SizeReductionService.TargetMaxHeight
  private val fileInputId     = "image-upload-file"

  val downscaleStrategies: List[DownscaleStrategy] =
    List(DownscaleAverage, DownscaleBayer.Size2, DownscaleBayer.Size4)
  val paletteColorCounts: List[Int] = (4 to 16).toList
  val colorDitheringOptions: List[ColorDithering] =
    List(
      NoColorDithering,
      FloydSteinbergDithering,
      OrderedBayerDithering.Size2,
      OrderedBayerDithering.Size4,
      OrderedBayerDithering.Size8)

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = ImageUploadModel(
      name = "Unnamed image",
      pixelPic = None,
      error = None,
      loading = false,
      sourceDataUrl = None,
      sourceFileName = None,
      downscaleStrategy = DownscaleAverage,
      paletteMode = UploadPaletteMode.Auto(16),
      colorDithering = NoColorDithering,
      pipelineRunId = 0L,
      savedPalettes = None,
      detectedColorCount = None
    )
    val fileCmd = CmdUtils.run(
      waitForFileSelection(),
      identity[ImageUploadMsg],
      e => ImageUploadMsg.ImageDecodedError(e.getMessage)
    )
    val palettesCmd = LocalStorageUtils.loadList(StorageKeys.palettes)(
      ImageUploadMsg.LoadedPalettes.apply,
      _ => ImageUploadMsg.LoadedPalettes(Nil),
      (_, _) => ImageUploadMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(fileCmd, palettesCmd))
  }

  private def waitForFileSelection(): IO[ImageUploadMsg] = {
    def findInput: IO[Input] =
      IO(Option(dom.document.getElementById(fileInputId)).map(_.asInstanceOf[Input]))
        .flatMap(IO.fromOption(_)(new NoSuchElementException("File input not found")))

    def addListener(input: Input): IO[ImageUploadMsg] =
      IO.async_[ImageUploadMsg] { cb =>
        input.addEventListener(
          "change",
          (_: dom.Event) => {
            val file = Option(input.files(0))
            input.value = ""
            file.foreach { f =>
              ImageUtils.loadImageFromFile(f).unsafeRunAsync {
                case Right((name, dataUrl)) =>
                  cb(Right(ImageUploadMsg.FileLoaded(dataUrl, name)))
                case Left(err) =>
                  cb(Right(ImageUploadMsg.ImageDecodedError(err.getMessage)))
              }
            }
          }
        )
      }

    IO.sleep(150.millis).flatMap(_ => findInput).flatMap(addListener)
  }

  private def runPipeline(model: Model): Cmd[IO, Msg] =
    (model.sourceDataUrl, model.sourceFileName) match {
      case (Some(url), Some(fileName)) =>
        val runId = model.pipelineRunId
        val paletteMode: PaletteMode = model.paletteMode match {
          case UploadPaletteMode.Auto(n) => AutoQuantize(n)
          case UploadPaletteMode.FromSaved(id) =>
            val colors = model.savedPalettes.flatMap(_.find(_.id == id)).map(_.colors).getOrElse(Vector.empty)
            FromPalette(colors.map(c => (c.r.toByte, c.g.toByte, c.b.toByte, 255.toByte)))
        }
        CmdUtils.run(
          PixelPicService
            .processUploadedImage(
              url,
              fileName,
              model.downscaleStrategy,
              paletteMode,
              model.colorDithering
            )
            .map(e =>
              e.fold(
                msg => ImageUploadMsg.ImageDecodedError(msg, Some(runId)),
                { case (pic, detectedColors) =>
                  ImageUploadMsg.ImageDecoded(pic, Some(fileName), detectedColors, runId)
                }
              )),
          identity[ImageUploadMsg],
          e => ImageUploadMsg.ImageDecodedError(e.getMessage, Some(runId))
        )
      case _ =>
        Cmd.None
    }

  private def baseNameFromFileName(fileName: String): String = {
    val i = fileName.lastIndexOf('.')
    if (i <= 0) fileName else fileName.substring(0, i)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImageUploadMsg.FileLoaded(dataUrl, fileName) =>
      val next = model.copy(
        sourceDataUrl = Some(dataUrl),
        sourceFileName = Some(fileName),
        name = baseNameFromFileName(fileName),
        error = None,
        loading = true,
        pipelineRunId = model.pipelineRunId + 1L,
        detectedColorCount = None,
        paletteMode = UploadPaletteMode.Auto(16)
      )
      (next, runPipeline(next))

    case ImageUploadMsg.ImageDecoded(pic, fileName, detectedColors, runId) =>
      if (runId != model.pipelineRunId) (model, Cmd.None)
      else {
        val name =
          if (model.pixelPic.isEmpty)
            fileName.map(baseNameFromFileName).filter(_.nonEmpty).getOrElse("Unnamed image")
          else
            model.name
        // On first load, set palette mode default to min(detectedColors, 16)
        val updatedPaletteMode =
          if (model.detectedColorCount.isEmpty)
            UploadPaletteMode.Auto(detectedColors.min(16).max(ColorQuantizationService.MinColors))
          else
            model.paletteMode
        val needsRerun = model.detectedColorCount.isEmpty && (updatedPaletteMode match {
          case UploadPaletteMode.Auto(n) => n != 16 // initial pipeline ran with 16, smart default differs
          case _                         => false
        })
        val nextModel = model.copy(
          pixelPic = Some(pic),
          name = name,
          error = None,
          loading = needsRerun,
          detectedColorCount = Some(detectedColors),
          paletteMode = updatedPaletteMode,
          pipelineRunId = if (needsRerun) model.pipelineRunId + 1L else model.pipelineRunId
        )
        if (needsRerun) {
          (nextModel, runPipeline(nextModel))
        } else {
          val drawCmd = CmdUtils.fireAndForget(
            CanvasUtils.runAfterFrames(3)(drawPreview(pic)),
            ImageUploadMsg.NoOp,
            _ => ImageUploadMsg.NoOp
          )
          (nextModel, drawCmd)
        }
      }

    case ImageUploadMsg.ImageDecodedError(msg, runId) =>
      if (runId.exists(_ != model.pipelineRunId)) (model, Cmd.None)
      else (model.copy(error = Some(msg), loading = false), Cmd.None)

    case ImageUploadMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case ImageUploadMsg.SetDownscaleStrategy(s) =>
      val next = model.copy(downscaleStrategy = s, loading = true, pipelineRunId = model.pipelineRunId + 1L)
      (next, runPipeline(next))

    case ImageUploadMsg.SetPaletteModeAuto(n) =>
      val next =
        model.copy(paletteMode = UploadPaletteMode.Auto(n), loading = true, pipelineRunId = model.pipelineRunId + 1L)
      (next, runPipeline(next))

    case ImageUploadMsg.SetPaletteModeFromSaved(id) =>
      val hasPalette = id.nonEmpty && model.savedPalettes.exists(_.exists(_.id == id))
      if (hasPalette) {
        val next = model.copy(
          paletteMode = UploadPaletteMode.FromSaved(id),
          loading = true,
          pipelineRunId = model.pipelineRunId + 1L)
        (next, runPipeline(next))
      } else {
        // No valid palette selected; switch mode in the UI but don't run pipeline
        (model.copy(paletteMode = UploadPaletteMode.FromSaved(id)), Cmd.None)
      }

    case ImageUploadMsg.LoadedPalettes(list) =>
      (model.copy(savedPalettes = Some(list)), Cmd.None)

    case ImageUploadMsg.SetColorDithering(d) =>
      val next = model.copy(colorDithering = d, loading = true, pipelineRunId = model.pipelineRunId + 1L)
      (next, runPipeline(next))

    case ImageUploadMsg.Save =>
      model.pixelPic match {
        case None =>
          (model.copy(error = Some("No image to save")), Cmd.None)
        case Some(_) =>
          val cmd = LocalStorageUtils.loadList(StorageKeys.images)(
            ImageUploadMsg.LoadedForSave.apply,
            _ => ImageUploadMsg.LoadedForSave(Nil),
            (_, _) => ImageUploadMsg.LoadedForSave(Nil)
          )
          (model.copy(loading = true), cmd)
      }

    case ImageUploadMsg.LoadedForSave(list) =>
      model.pixelPic match {
        case Some(pic) =>
          val id      = "image-" + js.Date.now().toLong
          val stored  = StoredImage(id = id, name = model.name, pixelPic = pic)
          val newList = list :+ stored
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.images, newList)(
            _ => NavigateNext(ScreenId.ImagesId, None),
            (_, _) => ImageUploadMsg.SaveFailed
          )
          (model.copy(loading = false), saveCmd)
        case None =>
          (model.copy(loading = false), Cmd.None)
      }

    case ImageUploadMsg.SaveFailed =>
      (model.copy(loading = false, error = Some("Failed to save")), Cmd.None)

    case ImageUploadMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ImagesId, None)))

    case ImageUploadMsg.DrawPreview =>
      val cmd = model.pixelPic match {
        case Some(pic) => Cmd.SideEffect(drawPreview(pic))
        case None      => Cmd.None
      }
      (model, cmd)

    case ImageUploadMsg.NoOp =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def pillClass(selected: Boolean, extra: String): String =
    if (selected) s"pill $extra pill--selected" else s"pill $extra pill--unselected"

  private def drawPreview(pic: PixelPic): IO[Unit] =
    CanvasUtils.drawAfterViewReady("image-upload-preview", maxRetries = 100, delayMs = 1) { (canvas, ctx) =>
      canvas.width = pic.width
      canvas.height = pic.height
      ctx.clearRect(0, 0, pic.width, pic.height)
      clemniem.common.CanvasUtils.drawPixelPic(canvas, ctx, pic, pic.width, pic.height, 0, 0)
    }

  def view(model: Model): Html[Msg] =
    div(
      `class` := s"${NesCss.screenContainer} screen-container--upload"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row", style := "gap: 0.5rem;")(
          GalleryLayout.backButton(ImageUploadMsg.Back, "Images"),
          label(
            `for`   := fileInputId,
            `class` := s"${NesCss.btnPrimary} label-as-button"
          )(text("Upload")),
          input(
            id      := fileInputId,
            `type`  := "file",
            `class` := "hidden"
          ),
          button(
            `class` := (if (model.pixelPic.nonEmpty && !model.loading) NesCss.btnSuccess
                        else s"${NesCss.btn} btn-disabled"),
            onClick(ImageUploadMsg.Save)
          )(text(if (model.loading) "Saving…" else "Save"))
        ),
        None,
        true
      ),
      p(`class` := s"${NesCss.text} screen-intro screen-intro--short")(
        text(
          s"Upload an image (max ${SizeReductionService.MaxUploadWidth}×${SizeReductionService.MaxUploadHeight} px). It will be resized to at most ${targetMaxWidth}×${targetMaxHeight} px.")
      ),
      model.error
        .map(err => div(`class` := s"${NesCss.container} ${NesCss.containerRounded} error-box")(text(err)))
        .getOrElse(div(`class` := "hidden")(text(""))),
      model.pixelPic match {
        case Some(pic) =>
          val colorCount = pic.paletteLookup.size
          div(`class` := "upload-settings-rows")(
            nameRow(model),
            downscaleBlock(model),
            paletteColorsBlock(model),
            colorDitheringBlock(model),
            div(`class` := "field-block--lg")(
              div(`class` := "upload-preview-title")(text("Preview"))
            ),
            div(`class` := "field-block")(
              div(`class` := "upload-preview-meta")(text(s"${pic.width}×${pic.height} px · $colorCount colors"))
            ),
            div(
              `class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card field-block upload-preview-row")(
              PixelPreviewBox("image-upload-preview", pic.width, pic.height, None)
            )
          )
        case None =>
          div(`class` := s"${NesCss.container} empty-state")(
            tyrian.Html.span(`class` := NesCss.text)(text("Click Upload and choose an image."))
          )
      }
    )

  private def nameRow(model: Model): Html[Msg] =
    div(`class` := "field-block--lg")(
      label(`class` := "label-block")(text("Name")),
      input(
        `type`  := "text",
        `class` := s"${NesCss.input} input-w-full",
        value   := model.name,
        onInput(ImageUploadMsg.SetName.apply)
      )
    )

  private def downscaleBlock(model: Model): Html[Msg] =
    div(`class` := "field-block--lg")(
      label(`class` := "label-block")(text("Downscale style")),
      div(`class` := "upload-option-row upload-option-row--wrap")(
        downscaleStrategies.map { s =>
          val selected = model.downscaleStrategy == s
          button(
            `class` := pillClass(selected, "upload-option-pill"),
            onClick(ImageUploadMsg.SetDownscaleStrategy(s))
          )(text(s.name))
        }*
      )
    )

  private def paletteColorsBlock(model: Model): Html[Msg] = {
    val isAuto      = model.paletteMode match { case UploadPaletteMode.Auto(_) => true; case _ => false }
    val isFromSaved = !isAuto
    // When switching to "From palette", pick the first saved palette as default
    val firstPaletteId = model.savedPalettes.flatMap(_.headOption).map(_.id).getOrElse("")

    div(`class` := "field-block--lg")(
      label(`class` := "label-block")(text("Palette")),
      // Mode selector row: Auto / From palette
      div(`class` := "upload-option-row upload-option-row--wrap")(
        button(
          `class` := pillClass(isAuto, "upload-option-pill"),
          onClick(
            ImageUploadMsg.SetPaletteModeAuto(
              model.paletteMode match {
                case UploadPaletteMode.Auto(n) => n;
                case _ => model.detectedColorCount.map(_.min(16).max(ColorQuantizationService.MinColors)).getOrElse(16)
              }
            ))
        )(text("Auto")),
        button(
          `class` := pillClass(isFromSaved, "upload-option-pill"),
          onClick(
            ImageUploadMsg.SetPaletteModeFromSaved(
              model.paletteMode match {
                case UploadPaletteMode.FromSaved(id) => id; case _ => firstPaletteId
              }
            ))
        )(text("From palette"))
      ),
      // Conditional detail row
      model.paletteMode match {
        case UploadPaletteMode.Auto(n) =>
          div()(
            div(`class` := "upload-option-row upload-option-row--wrap", style := "margin-top: 0.35rem;")(
              paletteColorCounts.map { count =>
                val selected = count == n
                button(
                  `class` := pillClass(selected, "upload-option-pill"),
                  onClick(ImageUploadMsg.SetPaletteModeAuto(count))
                )(text(count.toString))
              }*
            ),
            model.pixelPic
              .map { pic =>
                div(`class` := "upload-palette-strip", style := "margin-top: 0.35rem;")(
                  PaletteStripView.previewInline(pic.paletteLookup.toList.map(p => Color(p.r, p.g, p.b)))
                )
              }
              .getOrElse(div(`class` := "hidden")(text(""))),
            span(`class` := "helper-text helper-text--top")(text("Reduce to 4–16 colors."))
          )
        case UploadPaletteMode.FromSaved(selectedId) =>
          model.savedPalettes match {
            case Some(palettes) if palettes.nonEmpty =>
              div()(
                div(`class` := "upload-option-row", style := "margin-top: 0.35rem;")(
                  tyrian.Html.select(
                    `class` := s"${NesCss.input} input-w-full",
                    onChange(v => ImageUploadMsg.SetPaletteModeFromSaved(v))
                  )(
                    palettes.map { sp =>
                      val attrs =
                        if (sp.id == selectedId) List(Attribute("value", sp.id), selected(true))
                        else List(Attribute("value", sp.id))
                      option(attrs*)(text(s"${sp.name} (${sp.colors.size} colors)"))
                    }*
                  )
                ),
                palettes
                  .find(_.id == selectedId)
                  .map { sp =>
                    div(`class` := "upload-palette-strip", style := "margin-top: 0.35rem;")(
                      PaletteStripView.previewInline(sp.colors.toList)
                    )
                  }
                  .getOrElse(div(`class` := "hidden")(text(""))),
                span(`class` := "helper-text helper-text--top")(text("Map image colors to this saved palette."))
              )
            case _ =>
              div(style := "margin-top: 0.35rem;")(
                span(`class` := "helper-text")(text("No palettes saved yet. Save a palette first."))
              )
          }
      }
    )
  }

  private def colorDitheringBlock(model: Model): Html[Msg] =
    div(`class` := "field-block--lg")(
      label(`class` := "label-block")(text("Color dithering")),
      div(`class` := "upload-option-row upload-option-row--wrap")(
        colorDitheringOptions.map { d =>
          val selected = model.colorDithering.name == d.name
          button(
            `class` := pillClass(selected, "upload-option-pill"),
            onClick(ImageUploadMsg.SetColorDithering(d))
          )(text(d.name))
        }*
      ),
      span(`class` := "helper-text helper-text--top")(text("Dithering pattern for mapping to the palette."))
    )

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
}

/** Palette mode selection in the upload UI. */
enum UploadPaletteMode {
  case Auto(numColors: Int)
  case FromSaved(paletteId: String)
}

final case class ImageUploadModel(
  name: String,
  pixelPic: Option[PixelPic],
  error: Option[String],
  loading: Boolean,
  sourceDataUrl: Option[String],
  sourceFileName: Option[String],
  downscaleStrategy: DownscaleStrategy,
  paletteMode: UploadPaletteMode,
  colorDithering: ColorDithering,
  pipelineRunId: Long,
  savedPalettes: Option[List[StoredPalette]],
  detectedColorCount: Option[Int])

enum ImageUploadMsg {
  case FileLoaded(dataUrl: String, fileName: String)
  case ImageDecoded(pic: PixelPic, fileName: Option[String], detectedColors: Int, runId: Long)
  case ImageDecodedError(message: String, runId: Option[Long] = None)
  case SetName(name: String)
  case SetDownscaleStrategy(strategy: DownscaleStrategy)
  case SetPaletteModeAuto(numColors: Int)
  case SetPaletteModeFromSaved(paletteId: String)
  case LoadedPalettes(list: List[StoredPalette])
  case SetColorDithering(dithering: ColorDithering)
  case Save
  case LoadedForSave(list: List[StoredImage])
  case SaveFailed
  case DrawPreview
  case Back
  case NoOp
}
