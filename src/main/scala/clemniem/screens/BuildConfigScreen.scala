package clemniem.screens

import cats.effect.IO
import clemniem.{
  BuildConfig,
  Layout,
  PixelPic,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuildConfig,
  StoredImage,
  StoredLayout,
  StoredPalette
}
import clemniem.common.{CanvasUtils, CmdUtils, LocalStorageUtils, PixelPicCanvas}
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Build config editor: select one Layout, one Image, one Palette, and offset; preview updates when palette changes. */
object BuildConfigScreen extends Screen {
  type Model = BuildConfigModel
  type Msg   = BuildConfigMsg

  val screenId: ScreenId = ScreenId.BuildConfigId

  private val overviewCanvasId = "build-config-overview"
  private val previewCanvasId  = "build-config-preview"

  /** Resolve a [[PrefillSpec]] against the loaded lists to determine which IDs to preselect. */
  private def resolvePrefill(
    spec: PrefillSpec,
    grids: List[StoredLayout],
    images: List[StoredImage],
    palettes: List[StoredPalette]
  ): (Option[String], Option[String], Option[String]) =
    spec match {
      case PrefillSpec.FromExisting(gridConfig, imageRef, paletteRef) =>
        (
          gridConfig.flatMap(gc => grids.find(_.config == gc).map(_.id)),
          imageRef.filter(id => images.exists(_.id == id)).orElse(images.headOption.map(_.id)),
          paletteRef.filter(id => palettes.exists(_.id == id)).orElse(palettes.headOption.map(_.id))
        )
      case PrefillSpec.Empty =>
        (None, None, None)
    }

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val (name, ox, oy, editingId, prefill) = previous match {
      case Some(ScreenOutput.EditBuildConfig(stored)) =>
        (
          stored.name,
          stored.config.offsetX,
          stored.config.offsetY,
          Some(stored.id),
          PrefillSpec.FromExisting(
            gridConfig = Some(stored.config.grid),
            imageRef = Some(stored.config.imageRef),
            paletteRef = Some(stored.config.paletteRef)
          ))
      case _ =>
        ("Unnamed build", 0, 0, None, PrefillSpec.Empty)
    }
    val model = BuildConfigModel(
      layouts = None,
      images = None,
      palettes = None,
      selectedGridId = None,
      selectedImageId = None,
      selectedPaletteId = None,
      offsetX = ox,
      offsetY = oy,
      name = name,
      editingId = editingId,
      prefill = prefill
    )
    val loadGrid = LocalStorageUtils.loadList(StorageKeys.layouts)(
      BuildConfigMsg.LoadedLayouts.apply,
      (_, _) => BuildConfigMsg.LoadedLayouts(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildConfigMsg.LoadedImages.apply,
      (_, _) => BuildConfigMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildConfigMsg.LoadedPalettes.apply,
      (_, _) => BuildConfigMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadGrid, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildConfigMsg.LoadedLayouts(list) =>
      val next    = model.copy(layouts = Some(list))
      val sel     = resolvePrefill(model.prefill, list, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))
      val withSel = next.copy(selectedGridId = sel._1.orElse(next.selectedGridId).orElse(list.headOption.map(_.id)))
      val clamped = clampOffsets(withSel)
      (clamped, drawPreviewCmd(clamped))

    case BuildConfigMsg.LoadedImages(list) =>
      val next    = model.copy(images = Some(list))
      val sel     = resolvePrefill(model.prefill, model.layouts.getOrElse(Nil), list, model.palettes.getOrElse(Nil))
      val withSel = next.copy(selectedImageId = sel._2.orElse(next.selectedImageId).orElse(list.headOption.map(_.id)))
      val clamped = clampOffsets(withSel)
      (clamped, drawPreviewCmd(clamped))

    case BuildConfigMsg.LoadedPalettes(list) =>
      val next    = model.copy(palettes = Some(list))
      val sel     = resolvePrefill(model.prefill, model.layouts.getOrElse(Nil), model.images.getOrElse(Nil), list)
      val withSel =
        next.copy(selectedPaletteId = sel._3.orElse(next.selectedPaletteId).orElse(list.headOption.map(_.id)))
      val clamped = clampOffsets(withSel)
      (clamped, drawPreviewCmd(clamped))

    case BuildConfigMsg.SetGrid(id) =>
      val next = clampOffsets(model.copy(selectedGridId = Some(id)))
      (next, drawPreviewCmd(next))

    case BuildConfigMsg.SetImage(id) =>
      val next = clampOffsets(model.copy(selectedImageId = Some(id)))
      (next, drawPreviewCmd(next))

    case BuildConfigMsg.SetPalette(id) =>
      val next = model.copy(selectedPaletteId = Some(id))
      (next, drawPreviewCmd(next))

    case BuildConfigMsg.SetOffsetX(n) =>
      val next = clampOffsets(model.copy(offsetX = n))
      (next, drawPreviewCmd(next))

    case BuildConfigMsg.SetOffsetY(n) =>
      val next = clampOffsets(model.copy(offsetY = n))
      (next, drawPreviewCmd(next))

    case BuildConfigMsg.SetName(name) =>
      (model.copy(name = name), Cmd.None)

    case BuildConfigMsg.Save =>
      (for {
        grid    <- model.selectedGridId.flatMap(id => model.layouts.flatMap(_.find(_.id == id)))
        image   <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
        palette <- model.selectedPaletteId.flatMap(id => model.palettes.flatMap(_.find(_.id == id)))
      } yield BuildConfig(
        grid = grid.config,
        imageRef = image.id,
        paletteRef = palette.id,
        offsetX = model.offsetX,
        offsetY = model.offsetY
      )) match {
        case Some(config) =>
          val cmd = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
            BuildConfigMsg.LoadedForSave.apply,
            (_, _) => BuildConfigMsg.LoadedForSave(Nil)
          )
          (model, cmd)
        case None =>
          (model, Cmd.None)
      }

    case BuildConfigMsg.LoadedForSave(list) =>
      (for {
        grid    <- model.selectedGridId.flatMap(id => model.layouts.flatMap(_.find(_.id == id)))
        image   <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
        palette <- model.selectedPaletteId.flatMap(id => model.palettes.flatMap(_.find(_.id == id)))
      } yield {
        val id     = model.editingId.getOrElse(LocalStorageUtils.newId("buildconfig"))
        val stored = StoredBuildConfig(
          id = id,
          name = model.name,
          config = BuildConfig(grid.config, image.id, palette.id, model.offsetX, model.offsetY))
        val newList = model.editingId match {
          case Some(eid) => list.filterNot(_.id == eid) :+ stored
          case None      => list :+ stored
        }
        LocalStorageUtils.saveList(StorageKeys.buildConfigs, newList)(
          _ => BuildConfigMsg.Saved,
          (_, _) => BuildConfigMsg.SaveFailed
        )
      }) match {
        case Some(saveCmd) => (model, saveCmd)
        case None          => (model, Cmd.None)
      }

    case BuildConfigMsg.Saved =>
      (model, navCmd(ScreenId.BuildConfigsId, None))

    case BuildConfigMsg.SaveFailed =>
      (model, Cmd.None)

    case BuildConfigMsg.Back =>
      (model, navCmd(ScreenId.BuildConfigsId, None))
    case BuildConfigMsg.DrawPreview =>
      (model, drawPreviewCmd(model))
    case BuildConfigMsg.NoOp =>
      (model, Cmd.None)
  }

  private def drawPreviewCmd(model: Model): Cmd[IO, Msg] =
    CmdUtils.fireAndForget(
      drawOverview(model).flatMap(_ => drawPreviewRegion(model)),
      BuildConfigMsg.NoOp,
      _ => BuildConfigMsg.NoOp)

  /** Overview: full image with palette + grid overlay at offset. */
  private def drawOverview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1) { (canvas, ctx) =>
      val picOpt  = picWithPalette(model)
      val gridOpt = model.selectedGridId.flatMap(id => model.layouts.flatMap(_.find(_.id == id)))
      (picOpt, gridOpt) match {
        case (Some(pic), Some(storedGrid)) =>
          PixelPicCanvas.drawFullImageWithGrid(canvas, ctx, pic, storedGrid.config, model.offsetX, model.offsetY, 400)
        case (Some(pic), None) =>
          renderers.BuildConfigRenderer.drawFullImage(canvas, ctx, pic, 400)
        case _ =>
          CanvasUtils.drawPlaceholder(canvas, ctx, 400, 200, "Select an image and colors for preview")
      }
    }

  /** Preview: only the grid region of the image (cropped at offset), with grid overlay. */
  private def drawPreviewRegion(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(previewCanvasId, maxRetries = 100, delayMs = 1) { (canvas, ctx) =>
      val picOpt  = picWithPalette(model)
      val gridOpt = model.selectedGridId.flatMap(id => model.layouts.flatMap(_.find(_.id == id)))
      (picOpt, gridOpt) match {
        case (Some(pic), Some(storedGrid)) =>
          renderers.BuildConfigRenderer.drawCroppedRegion(
            canvas,
            ctx,
            pic,
            storedGrid.config,
            model.offsetX,
            model.offsetY,
            300)
        case _ =>
          CanvasUtils.drawPlaceholder(canvas, ctx, 300, 200, "Select image, colors and layout for preview")
      }
    }

  private def picWithPalette(model: Model): Option[PixelPic] =
    for {
      img     <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
      palette <- model.selectedPaletteId.flatMap(id => model.palettes.flatMap(_.find(_.id == id)))
    } yield clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)

  /** Max (offsetX, offsetY) so that the grid stays inside the image; (0, 0) if no image/grid or grid larger than image.
    */
  private def maxOffsets(model: Model): (Int, Int) =
    (for {
      grid <- model.selectedGridId.flatMap(id => model.layouts.flatMap(_.find(_.id == id)))
      img  <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
    } yield {
      val iw = img.pixelPic.width
      val ih = img.pixelPic.height
      val gw = grid.config.width
      val gh = grid.config.height
      ((iw - gw).max(0), (ih - gh).max(0))
    }).getOrElse((0, 0))

  private def clampOffsets(model: Model): Model = {
    val (maxX, maxY) = maxOffsets(model)
    model.copy(
      offsetX = model.offsetX.max(0).min(maxX),
      offsetY = model.offsetY.max(0).min(maxY)
    )
  }

  def view(model: Model): Html[Msg] =
    div(`class` := s"${NesCss.screenContainer} screen-container--wide")(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row")(
          GalleryLayout.backButton(BuildConfigMsg.Back, "Mosaic setup"),
          button(`class` := NesCss.btnPrimary, onClick(BuildConfigMsg.Save))(text("Save"))
        ),
        Some(ScreenHeader.nameRowInput(model.name, BuildConfigMsg.SetName.apply, None, "")),
        false
      ),
      selectRow(
        "Layout",
        model.layouts,
        model.selectedGridId,
        BuildConfigMsg.SetGrid.apply,
        (g: StoredLayout) => g.name,
        (g: StoredLayout) => g.id),
      selectRow(
        "Image",
        model.images,
        model.selectedImageId,
        BuildConfigMsg.SetImage.apply,
        (i: StoredImage) => i.name,
        (i: StoredImage) => i.id),
      selectRow(
        "Palette",
        model.palettes,
        model.selectedPaletteId,
        BuildConfigMsg.SetPalette.apply,
        (p: StoredPalette) => p.name,
        (p: StoredPalette) => p.id),
      offsetRow(model),
      div(`class` := "build-config-canvases")(
        div(`class` := "build-config-canvas-block")(
          div(`class` := "section-title")(text("Overview")),
          div(onLoad(BuildConfigMsg.DrawPreview))(
            canvas(id := overviewCanvasId, width := 400, height := 200, `class` := "pixel-canvas")()
          )
        ),
        div(`class` := "build-config-canvas-block")(
          div(`class` := "section-title")(text("Preview")),
          div(onLoad(BuildConfigMsg.DrawPreview))(
            canvas(id := previewCanvasId, width := 300, height := 200, `class` := "pixel-canvas")()
          )
        )
      )
    )

  private def offsetRow(model: Model): Html[Msg] = {
    val (maxX, maxY) = maxOffsets(model)
    div(`class` := s"${NesCss.field} field-block")(
      div(`class` := "offset-sliders")(
        div(`class` := "offset-slider-group")(
          label(`class` := "label-block")(
            span(`class` := NesCss.text)(text("Position X:")),
            span(`class` := "offset-value")(text(model.offsetX.toString))
          ),
          input(
            `type` := "range",
            min    := "0",
            max    := maxX.max(1).toString,
            value  := model.offsetX.min(maxX).toString,
            onInput(s => BuildConfigMsg.SetOffsetX(s.toIntOption.getOrElse(0)))
          )
        ),
        div(`class` := "offset-slider-group")(
          label(`class` := "label-block")(
            span(`class` := NesCss.text)(text("Position Y:")),
            span(`class` := "offset-value")(text(model.offsetY.toString))
          ),
          input(
            `type` := "range",
            min    := "0",
            max    := maxY.max(1).toString,
            value  := model.offsetY.min(maxY).toString,
            onInput(s => BuildConfigMsg.SetOffsetY(s.toIntOption.getOrElse(0)))
          )
        )
      )
    )
  }

  private def selectRow[A](
    labelText: String,
    listOpt: Option[List[A]],
    selectedId: Option[String],
    setMsg: String => BuildConfigMsg,
    nameOf: A => String,
    idOf: A => String
  ): Html[Msg] =
    div(`class` := s"${NesCss.field} field-block")(
      label(`class` := "label-block")(span(`class` := NesCss.text)(text(s"$labelText:"))),
      listOpt match {
        case None =>
          span(`class` := NesCss.text)(text("Loading…"))
        case Some(list) if list.isEmpty =>
          span(`class` := NesCss.text)(text(s"No ${labelText.toLowerCase}s saved."))
        case Some(list) =>
          val currentId = selectedId.orElse(list.headOption.map(idOf))
          select(
            `class` := s"${NesCss.input} input-min-w-16",
            value   := currentId.getOrElse(""),
            onInput(s => setMsg(if (s.isEmpty) list.headOption.map(idOf).getOrElse("") else s))
          )(
            list.map { item =>
              val id = idOf(item)
              option(value := id)(text(nameOf(item)))
            }*
          )
      }
    )
}

final case class BuildConfigModel(
  layouts: Option[List[StoredLayout]],
  images: Option[List[StoredImage]],
  palettes: Option[List[StoredPalette]],
  selectedGridId: Option[String],
  selectedImageId: Option[String],
  selectedPaletteId: Option[String],
  offsetX: Int,
  offsetY: Int,
  name: String,
  editingId: Option[String],
  prefill: PrefillSpec)

/** Data-only description of which selections to prefill when editing an existing build config. Replaces the function
  * `(List[...], ...) => (Option[String], ...)` that was stored in the model.
  */
enum PrefillSpec {
  case Empty
  case FromExisting(
    gridConfig: Option[Layout],
    imageRef: Option[String],
    paletteRef: Option[String])
}

enum BuildConfigMsg {
  case LoadedLayouts(list: List[StoredLayout])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case SetGrid(id: String)
  case SetImage(id: String)
  case SetPalette(id: String)
  case SetOffsetX(n: Int)
  case SetOffsetY(n: Int)
  case SetName(name: String)
  case Save
  case LoadedForSave(list: List[StoredBuildConfig])
  case Saved
  case SaveFailed
  case Back
  case DrawPreview
  case NoOp
}
