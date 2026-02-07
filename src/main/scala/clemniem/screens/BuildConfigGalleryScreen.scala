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
import clemniem.common.nescss.NesCss
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
    val model = BuildConfigGalleryModel(None, None, None, None)
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
    case BuildConfigGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case BuildConfigGalleryMsg.ConfirmDelete(id) =>
      model.buildConfigs match {
        case Some(list) =>
          val newList = list.filterNot(_.id == id)
          val saveCmd = LocalStorageUtils.saveList(StorageKeys.buildConfigs, newList)(
            _ => BuildConfigGalleryMsg.CancelDelete,
            (_, _) => BuildConfigGalleryMsg.CancelDelete
          )
          (model.copy(buildConfigs = Some(newList), pendingDeleteId = None), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case BuildConfigGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
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
              val scale = (previewWidth.toDouble / cropped.width).min(previewHeight.toDouble / cropped.height).min(1.0)
              val cw = (cropped.width * scale).toInt.max(1)
              val ch = (cropped.height * scale).toInt.max(1)
              canvas.width = cw
              canvas.height = ch
              ctx.clearRect(0, 0, cw, ch)
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
    val backBtn = button(`class` := NesCss.btn, onClick(BuildConfigGalleryMsg.Back))(text("← Overview"))
    model.buildConfigs match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = false)
      case Some(list) =>
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No build configs yet.", "+ Create BuildConfig", BuildConfigGalleryMsg.CreateNew)
          else
            div(`class` := "flex-col")(
              (list.map(item => entryCard(item, model.pendingDeleteId.contains(item.id))) :+
                button(`class` := NesCss.btnPrimary, onClick(BuildConfigGalleryMsg.CreateNew))(text("+ Create BuildConfig")))*
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = false)
    }
  }

  private def entryCard(item: StoredBuildConfig, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(onLoad(BuildConfigGalleryMsg.DrawPreview(item)))(
        canvas(
          id := s"buildconfig-preview-${item.id}",
          width := previewWidth,
          height := previewHeight,
          `class` := "gallery-preview-canvas"
        )()
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.grid.width}×${item.config.grid.height} · ${item.config.imageRef}")
        ),
        if (confirmingDelete)
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text nes-text")(text(s"Delete \"${item.name}\"?")),
            button(`class` := NesCss.btnError, style := "margin-right: 6px;", onClick(BuildConfigGalleryMsg.ConfirmDelete(item.id)))(text("Yes")),
            button(`class` := NesCss.btn, onClick(BuildConfigGalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := NesCss.btn, onClick(BuildConfigGalleryMsg.Edit(item)))(text("Edit")),
            button(`class` := NesCss.btnError, onClick(BuildConfigGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )

}

final case class BuildConfigGalleryModel(
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    pendingDeleteId: Option[String]
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
  case Delete(stored: StoredBuildConfig)
  case ConfirmDelete(id: String)
  case CancelDelete
  case Back
