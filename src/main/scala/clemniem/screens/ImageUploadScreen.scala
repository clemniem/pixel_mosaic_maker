package clemniem.screens

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem._
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import org.scalajs.dom
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js
import scala.concurrent.duration.*

/** Upload image from computer: max 400×400, preview, palette preview, auto downscale if nearest-neighbor scaled. */
object ImageUploadScreen extends Screen {
  type Model = ImageUploadModel
  type Msg   = ImageUploadMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ImageUploadId

  private val maxWidth  = 400
  private val maxHeight = 400
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
            PixelPic.loadPixelImageFromFile(f).unsafeRunAsync {
              case Right(opt) =>
                val msg = opt.fold[ImageUploadMsg](ImageUploadMsg.ImageDecodedError("Could not decode image"))(ImageUploadMsg.ImageDecoded.apply)
                cb(Right(msg))
              case Left(err) =>
                cb(Right(ImageUploadMsg.ImageDecodedError(err.getMessage)))
            }
          }
        })
      }

    IO.sleep(150.millis).flatMap(_ => findInput).flatMap(addListener)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImageUploadMsg.ImageDecoded(pic) =>
      val err = if (pic.width > maxWidth || pic.height > maxHeight)
        Some(s"Image must be at most ${maxWidth}×${maxHeight} px (got ${pic.width}×${pic.height})")
      else
        None
      val nextModel = model.copy(pixelPic = Some(pic), error = err, loading = false)
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
          (model.copy(error = Some(s"Image must be at most ${maxWidth}×${maxHeight} px")), Cmd.None)
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
      style := "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1rem;"
    )(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
        h2(style := "margin: 0;")(text("Upload image")),
        div(style := "display: flex; align-items: center; gap: 8px;")(
          button(style := "padding: 6px 12px; cursor: pointer;", onClick(ImageUploadMsg.Back))(
            text("← Images")
          ),
          label(
            `for` := fileInputId,
            style := "padding: 6px 14px; cursor: pointer; background: #1565c0; color: #fff; border: none; border-radius: 4px; font-weight: 500; display: inline-block;"
          )(text("Upload")),
          input(
            id := fileInputId,
            `type` := "file",
            style := "display: none;"
          ),
          input(
            `type` := "text",
            placeholder := "Name",
            value := model.name,
            onInput(ImageUploadMsg.SetName.apply),
            style := "padding: 6px 10px; width: 12rem; border: 1px solid #ccc; border-radius: 4px;"
          ),
          button(
            style := (if (model.pixelPic.isEmpty || model.pixelPic.exists(p => p.width > maxWidth || p.height > maxHeight) || model.loading)
              "padding: 6px 14px; cursor: not-allowed; background: #9e9e9e; color: #fff; border: none; border-radius: 4px; font-weight: 500;"
            else
              "padding: 6px 14px; cursor: pointer; background: #2e7d32; color: #fff; border: none; border-radius: 4px; font-weight: 500;"),
            onClick(ImageUploadMsg.Save)
          )(text(if (model.loading) "Saving…" else "Save"))
        )
      ),
      p(style := "color: #444; margin-bottom: 1rem;")(
        text(s"Upload an image from your computer. Max size ${maxWidth}×${maxHeight} px. Scaled-up pixel art is auto-detected and resized.")
      ),
      model.error.map(err =>
        div(style := "margin-bottom: 1rem; padding: 10px; background: #ffebee; border: 1px solid #c62828; border-radius: 6px; color: #b71c1c;")(
          text(err)
        )
      ).getOrElse(div(style := "display: none;")(text(""))),
      model.pixelPic match {
        case Some(pic) =>
          div(style := "margin-bottom: 1rem;")(
            div(style := "margin-bottom: 0.5rem; font-weight: 500;")(text(s"Preview · ${pic.width}×${pic.height} px")),
            div()(
              canvas(
                id := "image-upload-preview",
                width := pic.width,
                height := pic.height,
                style := "border: 1px solid #333; display: block; max-width: 100%; image-rendering: pixelated; image-rendering: crisp-edges;"
              )()
            ),
            div(style := "margin-top: 0.5rem; font-weight: 500;")(text("Palette in image")),
            div(
              style := "display: flex; flex-wrap: wrap; gap: 4px; margin-top: 4px;"
            )(
              pic.paletteLookup.toList.map(c =>
                div(
                  style := s"width: 24px; height: 24px; border-radius: 2px; border: 1px solid #666; background: rgb(${c.r},${c.g},${c.b});"
                )()
              )*
            )
          )
        case None =>
          div(style := "padding: 2rem; border: 2px dashed #ccc; border-radius: 8px; background: #fafafa; text-align: center; color: #666;")(
            text("Click Upload and choose an image.")
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
  case ImageDecoded(pic: PixelPic)
  case ImageDecodedError(message: String)
  case SetName(name: String)
  case Save
  case LoadedForSave(list: List[StoredImage])
  case SaveFailed
  case DrawPreview
  case Back
  case NoOp