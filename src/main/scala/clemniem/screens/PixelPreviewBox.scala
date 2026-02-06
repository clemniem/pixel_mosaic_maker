package clemniem.screens

import tyrian.Html.*
import tyrian.*

/** Reusable pixel preview box: same canvas wrapper and styling as the image gallery preview.
  * Renders a wrapper div (with optional onLoad) and a canvas with class gallery-preview-canvas.
  *
  * @param canvasId  id for the canvas element
  * @param width     canvas width
  * @param height    canvas height
  * @param onLoadMsg if defined, the wrapper emits this message when mounted (e.g. to trigger drawing)
  */
object PixelPreviewBox {

  def apply[Msg](canvasId: String, canvasWidth: Int, canvasHeight: Int, onLoadMsg: Option[Msg]): Html[Msg] = {
    val canvasEl =
      canvas(
        id := canvasId,
        width := canvasWidth,
        height := canvasHeight,
        `class` := "gallery-preview-canvas"
      )()
    onLoadMsg match {
      case Some(msg) => div(onLoad(msg))(canvasEl)
      case None      => div()(canvasEl)
    }
  }
}
