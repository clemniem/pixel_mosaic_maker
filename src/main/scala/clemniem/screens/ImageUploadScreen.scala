package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.{Color, NavigateNext, PixelPic, Screen, ScreenId, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js
import scala.concurrent.duration.*

/** Upload image from computer: max 500×500, preview, palette preview, auto downscale if nearest-neighbor scaled. */
object ImageUploadScreen extends Screen {
  type Model = ImageUploadModel
  type Msg   = ImageUploadMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ImageUploadId

  private val maxWidth  = 500
  private val maxHeight = 500
  private val fileInputId = "image-upload-file"

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = ImageUploadModel(
      name = "Unnamed image",
      pixelPic = None,
      error = None,
      loading = false
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
            val fileName = Option(f.name).filter(_.nonEmpty)
            PixelPic.loadPixelImageFromFile(f).unsafeRunAsync {
              case Right(opt) =>
                val msg = opt.fold[ImageUploadMsg](ImageUploadMsg.ImageDecodedError("Could not decode image"))(pic =>
                  ImageUploadMsg.ImageDecoded(pic, fileName))
                cb(Right(msg))
              case Left(err) =>
                cb(Right(ImageUploadMsg.ImageDecodedError(err.getMessage)))
            }
          }
        })
      }

    IO.sleep(150.millis).flatMap(_ => findInput).flatMap(addListener)
  }

  private def baseNameFromFileName(fileName: String): String = {
    val i = fileName.lastIndexOf('.')
    if (i <= 0) fileName else fileName.substring(0, i)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImageUploadMsg.ImageDecoded(pic, fileName) =>
      val err = if (pic.width > maxWidth || pic.height > maxHeight)
        Some(s"Image can be at most ${maxWidth}×${maxHeight} pixels (got ${pic.width}×${pic.height})")
      else
        None
      val name = fileName.map(baseNameFromFileName).filter(_.nonEmpty).getOrElse("Unnamed image")
      val nextModel = model.copy(pixelPic = Some(pic), name = name, error = err, loading = false)
      val drawCmd   = Cmd.Run(
        CanvasUtils.runAfterFrames(3)(drawPreview(pic)).as(ImageUploadMsg.NoOp),
        (m: ImageUploadMsg) => m
      )
      (nextModel, drawCmd)

    case ImageUploadMsg.ImageDecodedError(msg) =>
      (model.copy(error = Some(msg), loading = false), Cmd.None)

    case ImageUploadMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case ImageUploadMsg.Save =>
      model.pixelPic match {
        case None =>
          (model.copy(error = Some("No image to save")), Cmd.None)
        case Some(pic) if pic.width > maxWidth || pic.height > maxHeight =>
          (model.copy(error = Some(s"Image can be at most ${maxWidth}×${maxHeight} pixels")), Cmd.None)
        case Some(pic) =>
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

  private def drawPreview(pic: PixelPic): IO[Unit] =
    CanvasUtils.drawAfterViewReady("image-upload-preview", maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      canvas.width = pic.width
      canvas.height = pic.height
      ctx.clearRect(0, 0, pic.width, pic.height)
      clemniem.common.CanvasUtils.drawPixelPic(canvas, ctx, pic, pic.width, pic.height)
    })

  def view(model: Model): Html[Msg] =
    div(
      `class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container"
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
            `class` := (if (model.pixelPic.nonEmpty && model.pixelPic.forall(p => p.width <= maxWidth && p.height <= maxHeight) && !model.loading)
              NesCss.btnSuccess else s"${NesCss.btn} btn-disabled"),
            onClick(ImageUploadMsg.Save)
          )(text(if (model.loading) "Saving…" else "Save"))
        ),
        if (model.pixelPic.isDefined) Some(ScreenHeader.nameRowInput(model.name, ImageUploadMsg.SetName.apply, None, ""))
        else None,
        true
      ),
      p(`class` := s"${NesCss.text} screen-intro screen-intro--short")(
        text(s"Upload an image from your computer. Max size ${maxWidth}×${maxHeight} px. Scaled-up pixel art is auto-detected and resized.")
      ),
      model.error.map(err =>
        div(`class` := s"${NesCss.container} ${NesCss.containerRounded} error-box")(text(err))
      ).getOrElse(div(`class` := "hidden")(text(""))),
      model.pixelPic match {
        case Some(pic) =>
          val colorCount = pic.paletteLookup.size
          div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card field-block")(
            PixelPreviewBox("image-upload-preview", pic.width, pic.height, None),
            div(`class` := "gallery-card-body")(
              div(`class` := "upload-preview-title")(text("Preview")),
              div(`class` := "upload-preview-meta")(text(s"${pic.width}×${pic.height} px · $colorCount colors")),
              div(style := "margin-top: 0.35rem;")(
                PaletteStripView.previewInline(pic.paletteLookup.toList.map(p => Color(p.r, p.g, p.b)))
              )
            )
          )
        case None =>
          div(`class` := s"${NesCss.container} empty-state")(
            tyrian.Html.span(`class` := NesCss.text)(text("Click Upload and choose an image."))
          )
      }
    )

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
}

final case class ImageUploadModel(
    name: String,
    pixelPic: Option[PixelPic],
    error: Option[String],
    loading: Boolean
)

enum ImageUploadMsg:
  case ImageDecoded(pic: PixelPic, fileName: Option[String] = None)
  case ImageDecodedError(message: String)
  case SetName(name: String)
  case Save
  case LoadedForSave(list: List[StoredImage])
  case SaveFailed
  case DrawPreview
  case Back
  case NoOp