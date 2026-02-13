package clemniem.common

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import clemniem.Color
import clemniem.Layout
import clemniem.PixelPic
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.CanvasRenderingContext2D

import scala.concurrent.duration.*

/** Workaround for drawing on a canvas before it is in the DOM (see gbcamutil):
  * - Defer draw to the next animation frame so the view/DOM has been committed (Event/View loop).
  * - Retry until the element is found.
  * Use [[drawAfterViewReady]] for all canvas updates so drawing is consistent.
  */
object CanvasUtils {

  /** Run `io` after the next requestAnimationFrame, so the browser has committed DOM updates (view loop). */
  def runAfterNextFrame(io: IO[Unit]): IO[Unit] =
    IO.async_[Unit] { cb =>
      val _ = dom.window.requestAnimationFrame((_: Double) => {
        io.unsafeRunAsync {
          case Right(()) => cb(Right(()))
          case Left(t)   => cb(Left(t))
        }
      })
    }

  /** Run `io` after `n` animation frames. Use n=1 for gallery list canvases (faster); use n=2 if the canvas is not found yet. */
  def runAfterFrames(n: Int)(io: IO[Unit]): IO[Unit] =
    if (n <= 0) io
    else runAfterNextFrame(runAfterFrames(n - 1)(io))

  /** Standard gallery preview size (all galleries use 120×80). */
  val galleryPreviewWidth: Int  = 120
  val galleryPreviewHeight: Int = 80

  /** Draw on a gallery-preview canvas: waits 1 frame, retries 100× every 3 ms. */
  def drawGalleryPreview(id: String)(draw: (Canvas, CanvasRenderingContext2D) => Unit): IO[Unit] =
    drawAfterViewReadyDelayed(id, 1, 100, 3)(draw)

  /** Like [[drawAfterViewReady]] but waits for `framesToWait` frames before retrying. Use for canvases in lists
    * (e.g. gallery previews) where the element may appear later than the first frame.
    */
  def drawAfterViewReadyDelayed(
      id: String,
      framesToWait: Int,
      maxRetries: Int,
      delayMs: Int
  )(draw: (Canvas, CanvasRenderingContext2D) => Unit): IO[Unit] =
    runAfterFrames(framesToWait)(
      getCanvasCtxWithRetries(id, maxRetries, delayMs).flatMap { case (canvas, ctx) =>
        IO(draw(canvas, ctx))
      }
    )

  /** Draw on a canvas after the view has been applied. Use this for every canvas update:
    * 1. Waits for the next animation frame (so Tyrian's view patch has run).
    * 2. Retries until the canvas element exists.
    * 3. Runs your draw function. Do not replace the canvas in the view; keep a stable node and only draw.
    */
  def drawAfterViewReady(
      id: String,
      maxRetries: Int,
      delayMs: Int
  )(draw: (Canvas, CanvasRenderingContext2D) => Unit): IO[Unit] =
    runAfterNextFrame(
      getCanvasCtxWithRetries(id, maxRetries, delayMs).flatMap { case (canvas, ctx) =>
        IO(draw(canvas, ctx))
      }
    )

  /** Get canvas and 2D context by id, retrying until the element exists (e.g. after view has rendered). */
  def getCanvasCtxWithRetries(
      id: String,
      maxRetries: Int,
      delayMs: Int
  ): IO[(Canvas, CanvasRenderingContext2D)] = {
    def attempt: IO[(Canvas, CanvasRenderingContext2D)] =
      IO.fromOption(Option(dom.document.getElementById(id)))(new NoSuchElementException(s"Canvas '$id' not found."))
        .map { el =>
          val canvas = el.asInstanceOf[Canvas]
          val ctx   = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
          (canvas, ctx)
        }

    def loop(remaining: Int): IO[(Canvas, CanvasRenderingContext2D)] =
      attempt.handleErrorWith {
        case _: NoSuchElementException if remaining > 0 =>
          IO.sleep(delayMs.millis) *> loop(remaining - 1)
        case other =>
          IO.raiseError(other)
      }

    loop(maxRetries)
  }

  /** Result of scaling a source rectangle to fit inside a destination rectangle. */
  case class ScaledFit(scale: Double, width: Int, height: Int, offsetX: Int, offsetY: Int)

  /** Compute scale factor, scaled dimensions, and centering offsets for fitting srcW×srcH inside destW×destH.
    * @param maxScale upper bound on scale (e.g. 1.0 to avoid upscaling; Double.MaxValue for no limit)
    */
  def scaleToFit(srcW: Int, srcH: Int, destW: Int, destH: Int, maxScale: Double): ScaledFit = {
    val scale = (destW.toDouble / srcW).min(destH.toDouble / srcH).min(maxScale)
    val cw    = (srcW * scale).toInt.max(1)
    val ch    = (srcH * scale).toInt.max(1)
    ScaledFit(scale, cw, ch, (destW - cw) / 2, (destH - ch) / 2)
  }

  /** Resize the canvas and draw a placeholder with centred text (editor screens). */
  def drawPlaceholder(canvas: Canvas, ctx: CanvasRenderingContext2D, w: Int, h: Int, text: String): Unit = {
    canvas.width = w
    canvas.height = h
    ctx.fillStyle = "#eee"
    ctx.fillRect(0, 0, w, h)
    ctx.fillStyle = "#999"
    ctx.font = "14px \"Press Start 2P\", cursive"
    ctx.fillText(text, 12, h / 2)
  }

  /** Scale a full image to fit maxDim×maxDim, draw it, then overlay grid part strokes offset by (offsetX, offsetY). */
  def drawFullImageWithGrid(
      canvas: Canvas,
      ctx: CanvasRenderingContext2D,
      pic: PixelPic,
      grid: Layout,
      offsetX: Int,
      offsetY: Int,
      maxDim: Int
  ): Unit = {
    val fit = scaleToFit(pic.width, pic.height, maxDim, maxDim, 1.0)
    canvas.width = fit.width
    canvas.height = fit.height
    ctx.clearRect(0, 0, fit.width, fit.height)
    drawPixelPic(canvas, ctx, pic, fit.width, fit.height, 0, 0)
    ctx.strokeStyle = Color.errorStroke.rgba(0.8)
    ctx.lineWidth = 1
    val ox = (offsetX * fit.scale).toInt
    val oy = (offsetY * fit.scale).toInt
    grid.parts.foreach { part =>
      ctx.strokeRect(ox + part.x * fit.scale, oy + part.y * fit.scale, (part.width * fit.scale).max(1), (part.height * fit.scale).max(1))
    }
  }

  /** Clear the canvas and draw centered error/placeholder text in the gallery preview style. */
  def drawCenteredErrorText(ctx: CanvasRenderingContext2D, width: Int, height: Int, message: String): Unit = {
    ctx.clearRect(0, 0, width, height)
    ctx.fillStyle = "#999"
    ctx.font = "12px \"Press Start 2P\", cursive"
    ctx.textAlign = "center"
    ctx.fillText(message, width / 2, height / 2)
  }

  /** Draw a PixelPic onto the canvas, scaled to targetWidth×targetHeight. Uses an offscreen buffer and
    * imageSmoothingEnabled = false for crisp pixel art. If pic is empty, clears the canvas.
    * @param dx x offset (e.g. for centering: (canvasWidth - targetWidth) / 2)
    * @param dy y offset (e.g. for centering: (canvasHeight - targetHeight) / 2)
    */
  def drawPixelPic(
      canvas: Canvas,
      ctx: CanvasRenderingContext2D,
      pic: PixelPic,
      targetWidth: Int,
      targetHeight: Int,
      dx: Int,
      dy: Int
  ): Unit = {
    if (targetWidth <= 0 || targetHeight <= 0) ()
    else if (pic.width <= 0 || pic.height <= 0) {
      ctx.clearRect(0, 0, targetWidth, targetHeight)
    }
    else {
      val (tmp, tctx) = ImageUtils.createOffscreenCanvas(pic.width, pic.height)
      val imgData     = tctx.createImageData(pic.width, pic.height)
      pic.fillImageData(imgData)
      tctx.putImageData(imgData, 0, 0)
      ctx.imageSmoothingEnabled = false
      ctx.drawImage(tmp, 0, 0, pic.width, pic.height, dx, dy, targetWidth, targetHeight)
    }
  }
}
