package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, PixelPic, Screen, ScreenId, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved images. Empty state: "Upload". */
object ImagesGalleryScreen extends Screen {
  type Model = Option[List[StoredImage]]
  type Msg   = ImagesGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ImagesId

  private val previewWidth  = 120
  private val previewHeight = 80

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val cmd = LocalStorageUtils.loadList(StorageKeys.images)(
      ImagesGalleryMsg.Loaded.apply,
      _ => ImagesGalleryMsg.Loaded(Nil),
      (_, _) => ImagesGalleryMsg.Loaded(Nil)
    )
    (None, cmd)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ImagesGalleryMsg.Loaded(list) =>
      val drawPreviews =
        if (list.isEmpty) Cmd.None
        else
          Cmd.SideEffect(
            CanvasUtils.runAfterFrames(3)(
              list.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
            )
          )
      (Some(list), drawPreviews)
    case ImagesGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ImageUploadId, None)))
    case ImagesGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case ImagesGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    model match {
      case None =>
        div(style := container)(p(text("Loading…")))
      case Some(list) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Images")),
            button(style := "padding: 6px 12px; cursor: pointer;", onClick(ImagesGalleryMsg.Back))(
              text("← Overview")
            )
          ),
          if (list.isEmpty)
            emptyState("Upload", ImagesGalleryMsg.CreateNew)
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map(item => entryCard(item)) :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(ImagesGalleryMsg.CreateNew)
              )(text("Upload")))*
            )
        )
    }
  }

  private def entryCard(item: StoredImage): Html[Msg] =
    div(
      style := "display: flex; align-items: center; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(onLoad(ImagesGalleryMsg.DrawPreview(item)))(
        canvas(
          id := s"image-preview-${item.id}",
          width := previewWidth,
          height := previewHeight,
          style := "border: 1px solid #999; border-radius: 2px; flex-shrink: 0; image-rendering: pixelated; image-rendering: crisp-edges;"
        )()
      ),
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(s"${item.pixelPic.width}×${item.pixelPic.height} px · ${item.pixelPic.paletteLookup.size} color(s)")
        )
      )
    )

  private def drawPreview(stored: StoredImage): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"image-preview-${stored.id}",
      framesToWait = 2,
      maxRetries = 100,
      delayMs = 3
    )((canvas: Canvas, ctx: CanvasRenderingContext2D) => drawPixelPicScaled(canvas, ctx, stored.pixelPic))

  private def drawPixelPicScaled(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic): Unit = {
    canvas.width = previewWidth
    canvas.height = previewHeight
    ctx.clearRect(0, 0, previewWidth, previewHeight)
    if (pic.width > 0 && pic.height > 0) {
      val scale = (previewWidth.toDouble / pic.width).min(previewHeight.toDouble / pic.height)
      val imgData = ctx.createImageData(pic.width, pic.height)
      val data    = imgData.data
      for (i <- pic.pixels.indices) {
        val px     = pic.paletteLookup(pic.pixels(i))
        val offset = i * 4
        data(offset) = px.r
        data(offset + 1) = px.g
        data(offset + 2) = px.b
        data(offset + 3) = px.a
      }
      val scaled = dom.document.createElement("canvas").asInstanceOf[Canvas]
      scaled.width = pic.width
      scaled.height = pic.height
      val sctx = scaled.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      sctx.putImageData(imgData, 0, 0)
      ctx.imageSmoothingEnabled = false
      ctx.drawImage(scaled, 0, 0, pic.width, pic.height, 0, 0, (pic.width * scale).toInt, (pic.height * scale).toInt)
    }
  }

  private def emptyState(createLabel: String, createMsg: Msg): Html[Msg] =
    div(
      style := "border: 2px dashed #ccc; border-radius: 8px; padding: 2rem; text-align: center; background: #fafafa;"
    )(
      p(style := "color: #666; margin-bottom: 1rem;")(text("No images yet.")),
      button(
        style := "padding: 10px 20px; font-size: 1rem; cursor: pointer; background: #333; color: #fff; border: none; border-radius: 6px;",
        onClick(createMsg)
      )(text(createLabel))
    )
}

enum ImagesGalleryMsg:
  case Loaded(list: List[StoredImage])
  case CreateNew
  case DrawPreview(stored: StoredImage)
  case Back
