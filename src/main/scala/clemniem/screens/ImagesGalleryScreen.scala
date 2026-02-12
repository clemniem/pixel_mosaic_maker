package clemniem.screens

import cats.effect.IO
import clemniem.{Color, NavigateNext, PixelPic, Screen, ScreenId, ScreenOutput, StorageKeys, StoredImage}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

/** Gallery of saved images. Empty state: "Upload". */
object ImagesGalleryScreen extends Screen {
  type Model = ImagesGalleryModel
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
    (ImagesGalleryModel(None, None, currentPage = 1), cmd)
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
      val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
      val page       = GalleryLayout.clampPage(model.currentPage, totalPages)
      (model.copy(list = Some(list), currentPage = page), drawPreviews)
    case ImagesGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ImageUploadId, None)))
    case ImagesGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case ImagesGalleryMsg.DrawPreview(stored) =>
      (model, Cmd.SideEffect(drawPreview(stored)))
    case ImagesGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case ImagesGalleryMsg.ConfirmDelete(id) =>
      model.list match {
        case Some(list) =>
          val newList    = list.filterNot(_.id == id)
          val saveCmd    = LocalStorageUtils.saveList(StorageKeys.images, newList)(
            _ => ImagesGalleryMsg.CancelDelete,
            (_, _) => ImagesGalleryMsg.CancelDelete
          )
          val totalPages = GalleryLayout.totalPagesFor(newList.size, GalleryLayout.defaultPageSize)
          val page       = GalleryLayout.clampPage(model.currentPage, totalPages)
          (model.copy(list = Some(newList), pendingDeleteId = None, currentPage = page), saveCmd)
        case None =>
          (model.copy(pendingDeleteId = None), Cmd.None)
      }
    case ImagesGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case ImagesGalleryMsg.PreviousPage =>
      val next = model.copy(currentPage = (model.currentPage - 1).max(1))
      val cmd = next.list match {
        case Some(list) if list.nonEmpty =>
          Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
        case _ => Cmd.None
      }
      (next, cmd)
    case ImagesGalleryMsg.NextPage =>
      model.list match {
        case Some(list) =>
          val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
          val next       = model.copy(currentPage = (model.currentPage + 1).min(totalPages))
          val cmd = if (list.isEmpty) Cmd.None
          else Cmd.SideEffect(CanvasUtils.runAfterFrames(3)(drawPreviewsForCurrentPage(next)))
          (next, cmd)
        case None => (model, Cmd.None)
      }
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val backBtn = GalleryLayout.backButton(ImagesGalleryMsg.Back, "Overview")
    val nextBtn = GalleryLayout.nextButton(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None))
    model.list match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = true, Some(nextBtn))
      case Some(list) =>
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No images yet.", "Upload", ImagesGalleryMsg.CreateNew)
          else
            paginatedList(
              list,
              model.currentPage,
              button(`class` := NesCss.btnPrimary, onClick(ImagesGalleryMsg.CreateNew))(text("Upload")),
              item => entryCard(item, model.pendingDeleteId.contains(item.id))
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = true, Some(nextBtn))
    }
  }

  private def paginatedList(
      list: List[StoredImage],
      currentPage: Int,
      addAction: Html[Msg],
      entryCard: StoredImage => Html[Msg]
  ): Html[Msg] =
    GalleryLayout.paginatedListWith(
      list,
      currentPage,
      GalleryLayout.defaultPageSize,
      addAction,
      entryCard,
      ImagesGalleryMsg.PreviousPage,
      ImagesGalleryMsg.NextPage
    )

  private def entryCard(item: StoredImage, confirmingDelete: Boolean): Html[Msg] =
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"${item.pixelPic.width}×${item.pixelPic.height} px · ${item.pixelPic.paletteLookup.size} colors")
        ),
        paletteRow(item),
        if (confirmingDelete)
          GalleryLayout.galleryDeleteConfirm(
            s"Delete \"${item.name}\"?",
            ImagesGalleryMsg.ConfirmDelete(item.id),
            ImagesGalleryMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
            button(`class` := NesCss.btnError, onClick(ImagesGalleryMsg.Delete(item)))(text("Delete"))
          )
      ),
      div(`class` := "gallery-card-preview")(
        div(`class` := "gallery-preview-wrap")(
          PixelPreviewBox(
            s"image-preview-${item.id}",
            previewWidth,
            previewHeight,
            Some(ImagesGalleryMsg.DrawPreview(item))
          )
        )
      )
    )

  private def paletteRow(item: StoredImage): Html[Msg] = {
    val colors = item.pixelPic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toList
    val output = ScreenOutput.NewPaletteFromImage(
      name = item.name + " palette",
      colors = item.pixelPic.paletteLookup.map(p => Color(p.r, p.g, p.b)).toVector
    )
    div(style := "margin-top: 0.35rem;", title := "Click to save as palette")(
      button(`class` := s"${NesCss.btn} palette-button-inline", onClick(NavigateNext(ScreenId.PaletteId, Some(output))))(
        PaletteStripView.swatches(colors)*
      )
    )
  }

  private def drawPreviewsForCurrentPage(model: Model): IO[Unit] =
    model.list match {
      case Some(list) if list.nonEmpty =>
        val pageSize = GalleryLayout.defaultPageSize
        val start    = (model.currentPage - 1) * pageSize
        val slice    = list.slice(start, start + pageSize)
        slice.foldLeft(IO.unit)((acc, item) => acc.flatMap(_ => drawPreview(item)))
      case _ => IO.unit
    }

  private def drawPreview(stored: StoredImage): IO[Unit] =
    CanvasUtils.drawAfterViewReadyDelayed(
      id = s"image-preview-${stored.id}",
      framesToWait = 1,
      maxRetries = 100,
      delayMs = 3
    )((canvas: Canvas, ctx: CanvasRenderingContext2D) => drawPixelPicScaled(canvas, ctx, stored.pixelPic))

  private def drawPixelPicScaled(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic): Unit = {
    ctx.clearRect(0, 0, previewWidth, previewHeight)
    if (pic.width > 0 && pic.height > 0) {
      val scale  = (previewWidth.toDouble / pic.width).min(previewHeight.toDouble / pic.height)
      val cw     = (pic.width * scale).toInt.max(1)
      val ch     = (pic.height * scale).toInt.max(1)
      val offsetX = (previewWidth - cw) / 2
      val offsetY = (previewHeight - ch) / 2
      CanvasUtils.drawPixelPic(canvas, ctx, pic, cw, ch, offsetX, offsetY)
    }
  }

}

final case class ImagesGalleryModel(
    list: Option[List[StoredImage]],
    pendingDeleteId: Option[String],
    currentPage: Int
)

enum ImagesGalleryMsg:
  case Loaded(list: List[StoredImage])
  case CreateNew
  case DrawPreview(stored: StoredImage)
  case Delete(stored: StoredImage)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
