package clemniem.common

import cats.effect.IO
import cats.effect.unsafe.implicits.global
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
      val tmp = dom.document.createElement("canvas").asInstanceOf[Canvas]
      tmp.width = pic.width
      tmp.height = pic.height
      val tctx = tmp.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      tctx.putImageData(imgData, 0, 0)
      ctx.imageSmoothingEnabled = false
      ctx.drawImage(tmp, 0, 0, pic.width, pic.height, dx, dy, targetWidth, targetHeight)
    }
  }
}
