package clemniem.screens

import cats.effect.IO
import clemniem.{
  Color,
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

  private val previewWidth  = CanvasUtils.galleryPreviewWidth
  private val previewHeight = CanvasUtils.galleryPreviewHeight

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = BuildConfigGalleryModel(None, None, None, None, currentPage = 1)
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
      val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
      val next       = model.copy(buildConfigs = Some(list), currentPage = GalleryLayout.clampPage(model.currentPage, totalPages))
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
          val totalPages = GalleryLayout.totalPagesFor(newList.size, GalleryLayout.defaultPageSize)
          (model.copy(buildConfigs = Some(newList), pendingDeleteId = None, currentPage = GalleryLayout.clampPage(model.currentPage, totalPages)), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case BuildConfigGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case BuildConfigGalleryMsg.PreviousPage =>
      val next = model.copy(currentPage = (model.currentPage - 1).max(1))
      val cmd  = if (next.canDrawPreviews) Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next))) else Cmd.None
      (next, cmd)
    case BuildConfigGalleryMsg.NextPage =>
      model.buildConfigs match {
        case Some(list) =>
          val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
          val next       = model.copy(currentPage = (model.currentPage + 1).min(totalPages))
          val cmd     = if (next.canDrawPreviews) Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next))) else Cmd.None
          (next, cmd)
        case None => (model, Cmd.None)
      }
    case BuildConfigGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawAllPreviews(model: BuildConfigGalleryModel): IO[Unit] =
    model.buildConfigs.getOrElse(Nil).foldLeft(IO.unit)((acc, item) =>
      acc.flatMap(_ => drawBuildConfigPreview(item, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil)))
    )

  private def drawPreviewsForCurrentPage(model: BuildConfigGalleryModel): IO[Unit] = {
    val list = model.buildConfigs.getOrElse(Nil)
    if (list.isEmpty) IO.unit
    else {
      val pageSize = GalleryLayout.defaultPageSize
      val start    = (model.currentPage - 1) * pageSize
      val slice    = list.slice(start, start + pageSize)
      val images   = model.images.getOrElse(Nil)
      val palettes = model.palettes.getOrElse(Nil)
      slice.foldLeft(IO.unit)((acc, item) =>
        acc.flatMap(_ => drawBuildConfigPreview(item, images, palettes))
      )
    }
  }

  private def drawBuildConfigPreview(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): IO[Unit] =
    CanvasUtils.drawGalleryPreview(s"buildconfig-preview-${stored.id}")((canvas: Canvas, ctx: CanvasRenderingContext2D) => {
      clemniem.PaletteUtils.picForBuildConfig(stored, images, palettes) match {
        case Some(pic) =>
          val gw = stored.config.grid.width
          val gh = stored.config.grid.height
          pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh) match {
            case Some(cropped) =>
              val fit = CanvasUtils.scaleToFit(cropped.width, cropped.height, previewWidth, previewHeight, 1.0)
              ctx.clearRect(0, 0, previewWidth, previewHeight)
              CanvasUtils.drawPixelPic(canvas, ctx, cropped, fit.width, fit.height, fit.offsetX, fit.offsetY)
              ctx.strokeStyle = Color.errorStroke.rgba(0.7)
              ctx.lineWidth = 1
              stored.config.grid.parts.foreach { part =>
                ctx.strokeRect(fit.offsetX + part.x * fit.scale, fit.offsetY + part.y * fit.scale, (part.width * fit.scale).max(1), (part.height * fit.scale).max(1))
              }
            case None =>
              CanvasUtils.drawCenteredErrorText(ctx, previewWidth, previewHeight, "Grid out of bounds")
          }
        case _ =>
          CanvasUtils.drawCenteredErrorText(ctx, previewWidth, previewHeight, "Missing image/palette")
      }
    })

  def view(model: Model): Html[Msg] = {
    val backBtn = GalleryLayout.backButton(BuildConfigGalleryMsg.Back, "Overview")
    val nextBtn = GalleryLayout.nextButton(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None))
    model.buildConfigs match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = false, Some(nextBtn))
      case Some(list) =>
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No setups yet.", "+ New mosaic setup", BuildConfigGalleryMsg.CreateNew)
          else
            paginatedList(
              list,
              model.currentPage,
              button(`class` := NesCss.btnPrimary, onClick(BuildConfigGalleryMsg.CreateNew))(text("+ New mosaic setup")),
              item => entryCard(item, model.pendingDeleteId.contains(item.id))
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = false, Some(nextBtn))
    }
  }

  private def paginatedList(
      list: List[StoredBuildConfig],
      currentPage: Int,
      addAction: Html[Msg],
      entryCard: StoredBuildConfig => Html[Msg]
  ): Html[Msg] =
    GalleryLayout.paginatedListWith(
      list,
      currentPage,
      GalleryLayout.defaultPageSize,
      addAction,
      entryCard,
      BuildConfigGalleryMsg.PreviousPage,
      BuildConfigGalleryMsg.NextPage
    )

  private def entryCard(item: StoredBuildConfig, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-preview-wrap")(
        div(onLoad(BuildConfigGalleryMsg.DrawPreview(item)))(
          canvas(
            id := s"buildconfig-preview-${item.id}",
            width := previewWidth,
            height := previewHeight,
            `class` := "gallery-preview-canvas"
          )()
        )
      ),
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.config.grid.width}×${item.config.grid.height} · ${item.config.imageRef}")
        ),
        if (confirmingDelete)
          GalleryLayout.galleryDeleteConfirm(
            s"Delete \"${item.name}\"?",
            BuildConfigGalleryMsg.ConfirmDelete(item.id),
            BuildConfigGalleryMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
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
    pendingDeleteId: Option[String],
    currentPage: Int
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
  case PreviousPage
  case NextPage
  case Back
