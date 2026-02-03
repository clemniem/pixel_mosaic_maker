package clemniem.screens

import cats.effect.IO
import clemniem.{
  NavigateNext,
  PixelPic,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuildConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved build configs. Empty state: "+ Create BuildConfig". Shows Preview canvas per item. */
object BuildConfigGalleryScreen extends Screen {
  type Model = BuildConfigGalleryModel
  type Msg   = BuildConfigGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildConfigsId

  private val previewWidth  = 120
  private val previewHeight = 80

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = BuildConfigGalleryModel(None, None, None)
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      BuildConfigGalleryMsg.LoadedBuildConfigs.apply,
      _ => BuildConfigGalleryMsg.LoadedBuildConfigs(Nil),
      (_, _) => BuildConfigGalleryMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildConfigGalleryMsg.LoadedImages.apply,
      _ => BuildConfigGalleryMsg.LoadedImages(Nil),
      (_, _) => BuildConfigGalleryMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildConfigGalleryMsg.LoadedPalettes.apply,
      _ => BuildConfigGalleryMsg.LoadedPalettes(Nil),
      (_, _) => BuildConfigGalleryMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuildConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildConfigGalleryMsg.LoadedBuildConfigs(list) =>
      val next = model.copy(buildConfigs = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(drawAllPreviews(next)) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.DrawPreview(item) =>
      (model, Cmd.SideEffect(drawBuildConfigPreview(item, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))))
    case BuildConfigGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildConfigId, None)))
    case BuildConfigGalleryMsg.Edit(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildConfigId, Some(ScreenOutput.EditBuildConfig(stored)))))
    case BuildConfigGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawAllPreviews(model: BuildConfigGalleryModel): IO[Unit] =
    model.buildConfigs.getOrElse(Nil).foldLeft(IO.unit)((acc, item) =>
      acc.flatMap(_ => drawBuildConfigPreview(item, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil)))
    )

  private def drawBuildConfigPreview(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"buildconfig-preview-${stored.id}",
      framesToWait = 2,
      maxRetries = 100,
      delayMs = 3
    )((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
      val imgOpt     = images.find(_.id == stored.config.imageRef)
      val paletteOpt = palettes.find(_.id == stored.config.paletteRef)
      (imgOpt, paletteOpt) match {
        case (Some(img), Some(palette)) =>
          val pic = clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)
          val gw = stored.config.grid.width
          val gh = stored.config.grid.height
          pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
            case Some(cropped) =>
              canvas.width = previewWidth
              canvas.height = previewHeight
              ctx.clearRect(0, 0, previewWidth, previewHeight)
              val scale = (previewWidth.toDouble / cropped.width).min(previewHeight.toDouble / cropped.height).min(1.0)
              val cw = (cropped.width * scale).toInt.max(1)
              val ch = (cropped.height * scale).toInt.max(1)
              CanvasUtils.drawPixelPic(canvas, ctx, cropped, cw, ch)
              ctx.strokeStyle = "rgba(255,0,0,0.7)"
              ctx.lineWidth = 1
              val gsx = scale
              val gsy = scale
              stored.config.grid.parts.foreach { part =>
                ctx.strokeRect(part.x * gsx, part.y * gsy, (part.width * gsx).max(1), (part.height * gsy).max(1))
              }
            case None =>
              canvas.width = previewWidth
              canvas.height = previewHeight
              ctx.fillStyle = "#eee"
              ctx.fillRect(0, 0, previewWidth, previewHeight)
              ctx.fillStyle = "#999"
              ctx.font = "12px system-ui"
              ctx.fillText("Grid out of bounds", 8, previewHeight / 2)
          }
        case _ =>
          canvas.width = previewWidth
          canvas.height = previewHeight
          ctx.fillStyle = "#eee"
          ctx.fillRect(0, 0, previewWidth, previewHeight)
          ctx.fillStyle = "#999"
          ctx.font = "12px system-ui"
          ctx.fillText("Missing image/palette", 8, previewHeight / 2)
      }
    })

  def view(model: Model): Html[Msg] = {
    val container =
      "font-family: system-ui, sans-serif; max-width: 40rem; margin: 0 auto; padding: 1.5rem;"
    model.buildConfigs match {
      case None =>
        div(style := container)(p(text("Loading…")))
      case Some(list) =>
        div(style := container)(
          div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1rem;")(
            h1(style := "margin: 0;")(text("Build configs")),
            button(
              style := "padding: 6px 12px; cursor: pointer;",
              onClick(BuildConfigGalleryMsg.Back)
            )(text("← Overview"))
          ),
          if (list.isEmpty)
            GalleryEmptyState("No build configs yet.", "+ Create BuildConfig", BuildConfigGalleryMsg.CreateNew)
          else
            div(style := "display: flex; flex-direction: column; gap: 0.5rem;")(
              (list.map(item => entryCard(item)) :+ button(
                style := "margin-top: 0.5rem; padding: 8px 16px; cursor: pointer;",
                onClick(BuildConfigGalleryMsg.CreateNew)
              )(text("+ Create BuildConfig")))*
            )
        )
    }
  }

  private def entryCard(item: StoredBuildConfig): Html[Msg] =
    div(
      style := "display: flex; align-items: center; gap: 0.75rem; padding: 0.75rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
    )(
      div(onLoad(BuildConfigGalleryMsg.DrawPreview(item)))(
        canvas(
          id := s"buildconfig-preview-${item.id}",
          width := previewWidth,
          height := previewHeight,
          style := "border: 1px solid #999; border-radius: 2px; flex-shrink: 0; image-rendering: pixelated; image-rendering: crisp-edges;"
        )()
      ),
      div(style := "min-width: 0; flex: 1;")(
        span(style := "font-weight: 500;")(text(item.name)),
        span(style := "display: block; color: #666; font-size: 0.875rem; margin-top: 0.25rem;")(
          text(s"${item.config.grid.width}×${item.config.grid.height} · ${item.config.imageRef}")
        )
      ),
      button(
        style := "padding: 4px 10px; cursor: pointer; flex-shrink: 0;",
        onClick(BuildConfigGalleryMsg.Edit(item))
      )(text("Edit"))
    )

}

final case class BuildConfigGalleryModel(
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]]
) {
  def canDrawPreviews: Boolean =
    buildConfigs.isDefined && images.isDefined && palettes.isDefined
}

enum BuildConfigGalleryMsg:
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case DrawPreview(stored: StoredBuildConfig)
  case CreateNew
  case Edit(stored: StoredBuildConfig)
  case Back
