package clemniem.screens

import cats.effect.IO
import clemniem.{
  Color,
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
  type Msg   = BuildConfigGalleryMsg

  val screenId: ScreenId = ScreenId.BuildConfigsId

  private val previewWidth  = CanvasUtils.galleryPreviewWidth
  private val previewHeight = CanvasUtils.galleryPreviewHeight

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val model = BuildConfigGalleryModel(Gallery.initState, None, None)
    val loadBuildConfigs = Gallery.loadCmd(StorageKeys.buildConfigs, BuildConfigGalleryMsg.LoadedBuildConfigs.apply, (_, _) => BuildConfigGalleryMsg.LoadedBuildConfigs(Nil))
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildConfigGalleryMsg.LoadedImages.apply,
      (_, _) => BuildConfigGalleryMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildConfigGalleryMsg.LoadedPalettes.apply,
      (_, _) => BuildConfigGalleryMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuildConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildConfigGalleryMsg.LoadedBuildConfigs(list) =>
      val next = model.copy(gallery = Gallery.onLoaded(model.gallery, list, GalleryLayout.defaultPageSize))
      val cmd = if (next.canDrawPreviews) Cmd.SideEffect(drawAllPreviews(next)) else Cmd.None
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
      (model, navCmd(ScreenId.BuildConfigId, None))
    case BuildConfigGalleryMsg.Edit(stored) =>
      (model, navCmd(ScreenId.BuildConfigId, Some(ScreenOutput.EditBuildConfig(stored))))
    case BuildConfigGalleryMsg.Delete(stored) =>
      (model.copy(gallery = Gallery.onRequestDelete(model.gallery, stored.id)), Cmd.None)
    case BuildConfigGalleryMsg.ConfirmDelete(id) =>
      val (gs, cmd) = Gallery.onConfirmDelete(model.gallery, id, StorageKeys.buildConfigs, GalleryLayout.defaultPageSize, BuildConfigGalleryMsg.CancelDelete)
      (model.copy(gallery = gs), cmd)
    case BuildConfigGalleryMsg.CancelDelete =>
      (model.copy(gallery = Gallery.onCancelDelete(model.gallery)), Cmd.None)
    case BuildConfigGalleryMsg.PreviousPage =>
      val next = model.copy(gallery = Gallery.onPreviousPage(model.gallery))
      val cmd = if (next.canDrawPreviews) redrawCurrentPageCmd(next) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.NextPage =>
      val next = model.copy(gallery = Gallery.onNextPage(model.gallery, GalleryLayout.defaultPageSize))
      val cmd = if (next.canDrawPreviews) redrawCurrentPageCmd(next) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
  }

  def view(model: Model): Html[Msg] =
    Gallery.view(
      screenId.title,
      model.gallery,
      GalleryLayout.defaultPageSize,
      shortHeader = false,
      BuildConfigGalleryMsg.Back,
      navMsg(ScreenFlow.nextInOverviewOrder(screenId), None),
      BuildConfigGalleryMsg.PreviousPage,
      BuildConfigGalleryMsg.NextPage,
      GalleryEmptyState("No setups yet.", "+ New mosaic setup", BuildConfigGalleryMsg.CreateNew),
      button(`class` := NesCss.btnPrimary, onClick(BuildConfigGalleryMsg.CreateNew))(text("+ New mosaic setup")),
      entryCard
    )

  private def entryCard(item: StoredBuildConfig, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-preview-wrap")(
        div(onLoad(BuildConfigGalleryMsg.DrawPreview(item)))(
          canvas(
            id      := s"buildconfig-preview-${item.id}",
            width   := previewWidth,
            height  := previewHeight,
            `class` := "gallery-preview-canvas"
          )()
        )
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.grid.width}\u00d7${item.config.grid.height} \u00b7 ${item.config.imageRef}")
        ),
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          BuildConfigGalleryMsg.ConfirmDelete.apply,
          BuildConfigGalleryMsg.CancelDelete,
          button(`class` := NesCss.btn, onClick(BuildConfigGalleryMsg.Edit(item)))(text("Edit")),
          button(`class` := NesCss.btnError, onClick(BuildConfigGalleryMsg.Delete(item)))(text("Delete"))
        )
      )
    )

  private def drawAllPreviews(model: BuildConfigGalleryModel): IO[Unit] =
    model.gallery.items
      .getOrElse(Nil)
      .foldLeft(IO.unit)((acc, item) =>
        acc.flatMap(_ => drawBuildConfigPreview(item, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))))

  private def redrawCurrentPageCmd(model: BuildConfigGalleryModel): Cmd[IO, Msg] = {
    val list = model.gallery.items.getOrElse(Nil)
    if (list.isEmpty) Cmd.None
    else {
      val start    = (model.gallery.currentPage - 1) * GalleryLayout.defaultPageSize
      val slice    = list.slice(start, start + GalleryLayout.defaultPageSize)
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(
        slice.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawBuildConfigPreview(item, images, palettes)))))
    }
  }

  private def drawBuildConfigPreview(
    stored: StoredBuildConfig,
    images: List[StoredImage],
    palettes: List[StoredPalette]
  ): IO[Unit] =
    CanvasUtils.drawGalleryPreview(s"buildconfig-preview-${stored.id}")(
      (_: Canvas, ctx: CanvasRenderingContext2D) =>
        clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes) match {
          case Some(pic) =>
            val gw = stored.config.grid.width
            val gh = stored.config.grid.height
            pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
              case Some(cropped) =>
                val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, previewWidth, previewHeight, 1.0)
                ctx.clearRect(0, 0, previewWidth, previewHeight)
                CanvasUtils.drawPixelPic(ctx, cropped, fit.width, fit.height, fit.offsetX, fit.offsetY)
                ctx.strokeStyle = Color.errorStroke.rgba(0.7)
                ctx.lineWidth = 1
                stored.config.grid.parts.foreach { part =>
                  ctx.strokeRect(
                    fit.offsetX + part.x * fit.scale,
                    fit.offsetY + part.y * fit.scale,
                    (part.width * fit.scale).max(1),
                    (part.height * fit.scale).max(1))
                }
              case None =>
                CanvasUtils.drawCenteredErrorText(ctx, previewWidth, previewHeight, "Grid out of bounds")
            }
          case _ =>
            CanvasUtils.drawCenteredErrorText(ctx, previewWidth, previewHeight, "Missing image/palette")
        })
}

final case class BuildConfigGalleryModel(
  gallery: Gallery.State[StoredBuildConfig],
  images: Option[List[StoredImage]],
  palettes: Option[List[StoredPalette]]
) {
  def canDrawPreviews: Boolean =
    gallery.items.isDefined && images.isDefined && palettes.isDefined
}

enum BuildConfigGalleryMsg {
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case DrawPreview(stored: StoredBuildConfig)
  case CreateNew
  case Edit(stored: StoredBuildConfig)
  case Delete(stored: StoredBuildConfig)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
}
