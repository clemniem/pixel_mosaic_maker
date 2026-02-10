package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.{Color, NavigateNext, PixelPic, Screen, ScreenId, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, ImageUtils, LocalStorageUtils}
import clemniem.common.image.{
  ColorDithering,
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
    List(NoColorDithering, FloydSteinbergDithering, OrderedBayerDithering.Size2, OrderedBayerDithering.Size4, OrderedBayerDithering.Size8)

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = ImageUploadModel(
      name = "Unnamed image",
      pixelPic = None,
      error = None,
      loading = false,
      sourceDataUrl = None,
      sourceFileName = None,
      downscaleStrategy = DownscaleAverage,
      numPaletteColors = None,
      colorDithering = NoColorDithering,
      pipelineRunId = 0L
    )
    val cmd = Cmd.Run(
      waitForFileSelection(),
      (msg: ImageUploadMsg) => msg
    )
    (model, cmd)
  }

  private def waitForFileSelection(): IO[ImageUploadMsg] = {
    def findInput: IO[Input] =
      IO(Option(dom.document.getElementById(fileInputId)).map(_.asInstanceOf[Input]))
        .flatMap(IO.fromOption(_)(new NoSuchElementException("File input not found")))

    def addListener(input: Input): IO[ImageUploadMsg] =
      IO.async_[ImageUploadMsg] { cb =>
        input.addEventListener("change", (_: dom.Event) => {
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
        })
      }

    IO.sleep(150.millis).flatMap(_ => findInput).flatMap(addListener)
  }

  private def runPipeline(model: Model): Cmd[IO, Msg] =
    (model.sourceDataUrl, model.sourceFileName) match {
      case (Some(url), Some(fileName)) =>
        val runId = model.pipelineRunId
        Cmd.Run(
          PixelPic.processUploadedImage(
            url,
            fileName,
            model.downscaleStrategy,
            model.numPaletteColors,
            model.colorDithering
          ).map(e =>
            e.fold(
              msg => ImageUploadMsg.ImageDecodedError(msg, Some(runId)),
              pic => ImageUploadMsg.ImageDecoded(pic, Some(fileName), runId)
            )
          ),
          (m: ImageUploadMsg) => m
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
        pipelineRunId = model.pipelineRunId + 1L
      )
      (next, runPipeline(next))

    case ImageUploadMsg.ImageDecoded(pic, fileName, runId) =>
      if (runId != model.pipelineRunId) (model, Cmd.None)
      else {
        val name =
          if (model.pixelPic.isEmpty)
            fileName.map(baseNameFromFileName).filter(_.nonEmpty).getOrElse("Unnamed image")
          else
            model.name
        val nextModel = model.copy(pixelPic = Some(pic), name = name, error = None, loading = false)
        val drawCmd   = Cmd.Run(
          CanvasUtils.runAfterFrames(3)(drawPreview(pic)).as(ImageUploadMsg.NoOp),
          (m: ImageUploadMsg) => m
        )
        (nextModel, drawCmd)
      }

    case ImageUploadMsg.ImageDecodedError(msg, runId) =>
      if (runId.exists(_ != model.pipelineRunId)) (model, Cmd.None)
      else (model.copy(error = Some(msg), loading = false), Cmd.None)

    case ImageUploadMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case ImageUploadMsg.SetDownscaleStrategy(s) =>
      val next = model.copy(downscaleStrategy = s, loading = true, pipelineRunId = model.pipelineRunId + 1L)
      (next, runPipeline(next))

    case ImageUploadMsg.SetNumPaletteColors(opt) =>
      val next = model.copy(numPaletteColors = opt, loading = true, pipelineRunId = model.pipelineRunId + 1L)
      (next, runPipeline(next))

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
          val id   = "image-" + js.Date.now().toLong
          val stored = StoredImage(id = id, name = model.name, pixelPic = pic)
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

  private def pillClass(selected: Boolean, base: String): String =
    if (selected) s"$base $base--selected" else s"$base $base--unselected"

  private def drawPreview(pic: PixelPic): IO[Unit] =
    CanvasUtils.drawAfterViewReady("image-upload-preview", maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      canvas.width = pic.width
      canvas.height = pic.height
      ctx.clearRect(0, 0, pic.width, pic.height)
      clemniem.common.CanvasUtils.drawPixelPic(canvas, ctx, pic, pic.width, pic.height)
    })

  def view(model: Model): Html[Msg] =
    div(
      `class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container screen-container--upload"
    )(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row", style := "gap: 0.5rem;")(
          button(`class` := NesCss.btn, onClick(ImageUploadMsg.Back))(GalleryLayout.backButtonLabel("←", "Images")),
          label(
            `for` := fileInputId,
            `class` := s"${NesCss.btnPrimary} label-as-button"
          )(text("Upload")),
          input(
            id := fileInputId,
            `type` := "file",
            `class` := "hidden"
          ),
          button(
            `class` := (if (model.pixelPic.nonEmpty && !model.loading) NesCss.btnSuccess else s"${NesCss.btn} btn-disabled"),
            onClick(ImageUploadMsg.Save)
          )(text(if (model.loading) "Saving…" else "Save"))
        ),
        None,
        true
      ),
      p(`class` := s"${NesCss.text} screen-intro screen-intro--short")(
        text(s"Upload an image (max ${SizeReductionService.MaxUploadWidth}×${SizeReductionService.MaxUploadHeight} px). It will be resized to at most ${targetMaxWidth}×${targetMaxHeight} px.")
      ),
      model.error.map(err =>
        div(`class` := s"${NesCss.container} ${NesCss.containerRounded} error-box")(text(err))
      ).getOrElse(div(`class` := "hidden")(text(""))),
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
            div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card field-block upload-preview-row")(
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
        `type` := "text",
        `class` := s"${NesCss.input} input-w-full",
        value := model.name,
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

  private def paletteColorsBlock(model: Model): Html[Msg] =
    div(`class` := "field-block--lg")(
      label(`class` := "label-block")(text("Palette colors")),
      div(`class` := "upload-option-row upload-option-row--wrap")(
        (button(
          `class` := pillClass(model.numPaletteColors.isEmpty, "upload-option-pill"),
          onClick(ImageUploadMsg.SetNumPaletteColors(None))
        )(text("Full")) +: paletteColorCounts.map { n =>
          val selected = model.numPaletteColors.contains(n)
          button(
            `class` := pillClass(selected, "upload-option-pill"),
            onClick(ImageUploadMsg.SetNumPaletteColors(Some(n)))
          )(text(n.toString))
        }.toVector)*
      ),
      (for {
        n   <- model.numPaletteColors
        pic <- model.pixelPic
        if !model.loading || pic.paletteLookup.size <= paletteColorCounts.max
      } yield div(`class` := "upload-palette-strip", style := "margin-top: 0.35rem;")(
        PaletteStripView.previewInline(pic.paletteLookup.toList.map(p => Color(p.r, p.g, p.b)))
      )).getOrElse(div(`class` := "hidden")(text(""))),
      span(`class` := "helper-text helper-text--top")(text("Reduce to 4–16 colors for a retro look."))
    )

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
      span(`class` := "helper-text helper-text--top")(text("Only applies when palette colors is set."))
    )

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
}

final case class ImageUploadModel(
    name: String,
    pixelPic: Option[PixelPic],
    error: Option[String],
    loading: Boolean,
    sourceDataUrl: Option[String],
    sourceFileName: Option[String],
    downscaleStrategy: DownscaleStrategy,
    numPaletteColors: Option[Int],
    colorDithering: ColorDithering,
    pipelineRunId: Long
)

enum ImageUploadMsg:
  case FileLoaded(dataUrl: String, fileName: String)
  case ImageDecoded(pic: PixelPic, fileName: Option[String], runId: Long)
  case ImageDecodedError(message: String, runId: Option[Long] = None)
  case SetName(name: String)
  case SetDownscaleStrategy(strategy: DownscaleStrategy)
  case SetNumPaletteColors(numColors: Option[Int])
  case SetColorDithering(dithering: ColorDithering)
  case Save
  case LoadedForSave(list: List[StoredImage])
  case SaveFailed
  case DrawPreview
  case Back
  case NoOp