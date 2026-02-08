package clemniem.screens

import cats.effect.IO
import clemniem.{
  BuildConfig,
  GridConfig,
  NavigateNext,
  PixelPic,
  Screen,
  ScreenId,
  ScreenOutput,
  StorageKeys,
  StoredBuildConfig,
  StoredGridConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{CanvasUtils, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

/** Build config editor: select one GridConfig, one Image, one Palette, and offset; preview updates when palette changes. */
object BuildConfigScreen extends Screen {
  type Model = BuildConfigModel
  type Msg   = BuildConfigMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BuildConfigId

  private val overviewCanvasId = "build-config-overview"
  private val previewCanvasId  = "build-config-preview"

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val (name, ox, oy, editingId, prefill) = previous match {
      case Some(ScreenOutput.EditBuildConfig(stored)) =>
        val pf = (g: List[StoredGridConfig], i: List[StoredImage], p: List[StoredPalette]) => (
          g.find(_.config == stored.config.grid).map(_.id),
          Some(stored.config.imageRef).filter(id => i.exists(_.id == id)).orElse(i.headOption.map(_.id)),
          Some(stored.config.paletteRef).filter(id => p.exists(_.id == id)).orElse(p.headOption.map(_.id))
        )
        (stored.name, stored.config.offsetX, stored.config.offsetY, Some(stored.id), pf)
      case _ =>
        ("Unnamed build", 0, 0, None, (_: List[StoredGridConfig], _: List[StoredImage], _: List[StoredPalette]) => (None, None, None))
    }
    val model = BuildConfigModel(
      gridConfigs = None,
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
    val loadGrid = LocalStorageUtils.loadList(StorageKeys.gridConfigs)(
      BuildConfigMsg.LoadedGridConfigs.apply,
      _ => BuildConfigMsg.LoadedGridConfigs(Nil),
      (_, _) => BuildConfigMsg.LoadedGridConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      BuildConfigMsg.LoadedImages.apply,
      _ => BuildConfigMsg.LoadedImages(Nil),
      (_, _) => BuildConfigMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      BuildConfigMsg.LoadedPalettes.apply,
      _ => BuildConfigMsg.LoadedPalettes(Nil),
      (_, _) => BuildConfigMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadGrid, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BuildConfigMsg.LoadedGridConfigs(list) =>
      val next = model.copy(gridConfigs = Some(list))
      val sel  = model.prefill(list, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))
      val withSel = next.copy(selectedGridId = sel._1.orElse(next.selectedGridId).orElse(list.headOption.map(_.id)))
      val clamped = clampOffsets(withSel)
      (clamped, drawPreviewCmd(clamped))

    case BuildConfigMsg.LoadedImages(list) =>
      val next = model.copy(images = Some(list))
      val sel  = model.prefill(model.gridConfigs.getOrElse(Nil), list, model.palettes.getOrElse(Nil))
      val withSel = next.copy(selectedImageId = sel._2.orElse(next.selectedImageId).orElse(list.headOption.map(_.id)))
      val clamped = clampOffsets(withSel)
      (clamped, drawPreviewCmd(clamped))

    case BuildConfigMsg.LoadedPalettes(list) =>
      val next = model.copy(palettes = Some(list))
      val sel  = model.prefill(model.gridConfigs.getOrElse(Nil), model.images.getOrElse(Nil), list)
      val withSel = next.copy(selectedPaletteId = sel._3.orElse(next.selectedPaletteId).orElse(list.headOption.map(_.id)))
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
        grid   <- model.selectedGridId.flatMap(id => model.gridConfigs.flatMap(_.find(_.id == id)))
        image  <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
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
            _ => BuildConfigMsg.LoadedForSave(Nil),
            (_, _) => BuildConfigMsg.LoadedForSave(Nil)
          )
          (model, cmd)
        case None =>
          (model, Cmd.None)
      }

    case BuildConfigMsg.LoadedForSave(list) =>
      (for {
        grid   <- model.selectedGridId.flatMap(id => model.gridConfigs.flatMap(_.find(_.id == id)))
        image  <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
        palette <- model.selectedPaletteId.flatMap(id => model.palettes.flatMap(_.find(_.id == id)))
      } yield {
        val id   = model.editingId.getOrElse("buildconfig-" + js.Date.now().toLong)
        val stored = StoredBuildConfig(id = id, name = model.name, config = BuildConfig(grid.config, image.id, palette.id, model.offsetX, model.offsetY))
        val newList = model.editingId match {
          case Some(eid) => list.filterNot(_.id == eid) :+ stored
          case None      => list :+ stored
        }
        LocalStorageUtils.saveList(StorageKeys.buildConfigs, newList)(
          _ => NavigateNext(ScreenId.BuildConfigsId, None),
          (_, _) => BuildConfigMsg.SaveFailed
        )
      }) match {
        case Some(saveCmd) => (model, saveCmd)
        case None          => (model, Cmd.None)
      }

    case BuildConfigMsg.SaveFailed =>
      (model, Cmd.None)

    case BuildConfigMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.BuildConfigsId, None)))
    case BuildConfigMsg.DrawPreview =>
      (model, drawPreviewCmd(model))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def drawPreviewCmd(model: Model): Cmd[IO, Msg] =
    Cmd.SideEffect(drawOverview(model).flatMap(_ => drawPreviewRegion(model)))

  /** Overview: full image with palette + grid overlay at offset. */
  private def drawOverview(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(overviewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val picOpt = picWithPalette(model)
      val gridOpt = model.selectedGridId.flatMap(id => model.gridConfigs.flatMap(_.find(_.id == id)))
      (picOpt, gridOpt) match {
        case (Some(pic), Some(storedGrid)) =>
          drawFullImageWithGrid(canvas, ctx, pic, storedGrid.config, model.offsetX, model.offsetY)
        case (Some(pic), None) =>
          drawFullImageOnly(canvas, ctx, pic)
        case _ =>
          drawPlaceholder(canvas, ctx, 400, 200, "Select image and palette for overview")
      }
    })

  /** Preview: only the grid region of the image (cropped at offset), with grid overlay. */
  private def drawPreviewRegion(model: Model): IO[Unit] =
    CanvasUtils.drawAfterViewReady(previewCanvasId, maxRetries = 100, delayMs = 1)((canvas, ctx) => {
      val picOpt   = picWithPalette(model)
      val gridOpt  = model.selectedGridId.flatMap(id => model.gridConfigs.flatMap(_.find(_.id == id)))
      (picOpt, gridOpt) match {
        case (Some(pic), Some(storedGrid)) =>
          val gw = storedGrid.config.width
          val gh = storedGrid.config.height
          pic.crop(model.offsetX, model.offsetY, gw, gh) match {
            case Some(cropped) =>
              val scale = (300.0 / (cropped.width.max(cropped.height))).min(1.0)
              val cw = (cropped.width * scale).toInt.max(1)
              val ch = (cropped.height * scale).toInt.max(1)
              canvas.width = cw
              canvas.height = ch
              ctx.clearRect(0, 0, cw, ch)
              CanvasUtils.drawPixelPic(canvas, ctx, cropped, cw, ch)
              ctx.strokeStyle = "rgba(255,0,0,0.8)"
              ctx.lineWidth = 1
              val gsx = scale
              val gsy = scale
              storedGrid.config.parts.foreach { part =>
                ctx.strokeRect(part.x * gsx, part.y * gsy, (part.width * gsx).max(1), (part.height * gsy).max(1))
              }
            case None =>
              drawPlaceholder(canvas, ctx, 300, 200, "Grid region outside image bounds")
          }
        case _ =>
          drawPlaceholder(canvas, ctx, 300, 200, "Select image, palette and grid for preview")
      }
    })

  private def picWithPalette(model: Model): Option[PixelPic] =
    for {
      img     <- model.selectedImageId.flatMap(id => model.images.flatMap(_.find(_.id == id)))
      palette <- model.selectedPaletteId.flatMap(id => model.palettes.flatMap(_.find(_.id == id)))
    } yield clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)

  private def drawFullImageWithGrid(
      canvas: Canvas,
      ctx: CanvasRenderingContext2D,
      pic: PixelPic,
      grid: GridConfig,
      offsetX: Int,
      offsetY: Int
  ): Unit = {
    val scale = (400.0 / (pic.width.max(pic.height))).min(1.0)
    val cw = (pic.width * scale).toInt.max(1)
    val ch = (pic.height * scale).toInt.max(1)
    canvas.width = cw
    canvas.height = ch
    ctx.clearRect(0, 0, cw, ch)
    CanvasUtils.drawPixelPic(canvas, ctx, pic, cw, ch)
    ctx.strokeStyle = "rgba(255,0,0,0.8)"
    ctx.lineWidth = 1
    val ox  = (offsetX * scale).toInt
    val oy  = (offsetY * scale).toInt
    val gsx = scale
    val gsy = scale
    grid.parts.foreach { part =>
      ctx.strokeRect(ox + part.x * gsx, oy + part.y * gsy, (part.width * gsx).max(1), (part.height * gsy).max(1))
    }
  }

  private def drawFullImageOnly(canvas: Canvas, ctx: CanvasRenderingContext2D, pic: PixelPic): Unit = {
    val scale = (400.0 / (pic.width.max(pic.height))).min(1.0)
    val cw = (pic.width * scale).toInt.max(1)
    val ch = (pic.height * scale).toInt.max(1)
    canvas.width = cw
    canvas.height = ch
    ctx.clearRect(0, 0, cw, ch)
    CanvasUtils.drawPixelPic(canvas, ctx, pic, cw, ch)
  }

  /** Max (offsetX, offsetY) so that the grid stays inside the image; (0, 0) if no image/grid or grid larger than image. */
  private def maxOffsets(model: Model): (Int, Int) =
    (for {
      grid <- model.selectedGridId.flatMap(id => model.gridConfigs.flatMap(_.find(_.id == id)))
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

  private def drawPlaceholder(canvas: Canvas, ctx: CanvasRenderingContext2D, w: Int, h: Int, text: String): Unit = {
    canvas.width = w
    canvas.height = h
    ctx.fillStyle = "#eee"
    ctx.fillRect(0, 0, w, h)
    ctx.fillStyle = "#999"
    ctx.font = "14px \"Press Start 2P\", cursive"
    ctx.fillText(text, 12, h / 2)
  }

  def view(model: Model): Html[Msg] = {
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container screen-container--wide")(
      ScreenHeader(
        screenId.title,
        div(`class` := "flex-row")(
          button(`class` := NesCss.btn, onClick(BuildConfigMsg.Back))(text("← Build configs")),
          button(`class` := NesCss.btnPrimary, onClick(BuildConfigMsg.Save))(text("Save"))
        ),
        Some(ScreenHeader.nameRowInput(model.name, BuildConfigMsg.SetName.apply, None, "")),
        false
      ),
      selectRow("Grid", model.gridConfigs, model.selectedGridId, BuildConfigMsg.SetGrid.apply, (g: StoredGridConfig) => g.name, (g: StoredGridConfig) => g.id),
      selectRow("Image", model.images, model.selectedImageId, BuildConfigMsg.SetImage.apply, (i: StoredImage) => i.name, (i: StoredImage) => i.id),
      selectRow("Palette", model.palettes, model.selectedPaletteId, BuildConfigMsg.SetPalette.apply, (p: StoredPalette) => p.name, (p: StoredPalette) => p.id),
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
  }

  private def offsetRow(model: Model): Html[Msg] = {
    val (maxX, maxY) = maxOffsets(model)
    div(`class` := s"${NesCss.field} field-block")(
      div(`class` := "offset-sliders")(
        div(`class` := "offset-slider-group")(
          label(`class` := "label-block")(
            span(`class` := NesCss.text)(text("Offset X:")),
            span(`class` := "offset-value")(text(model.offsetX.toString))
          ),
          input(
            `type` := "range",
            min := "0",
            max := maxX.max(1).toString,
            value := model.offsetX.min(maxX).toString,
            onInput(s => BuildConfigMsg.SetOffsetX(s.toIntOption.getOrElse(0)))
          )
        ),
        div(`class` := "offset-slider-group")(
          label(`class` := "label-block")(
            span(`class` := NesCss.text)(text("Offset Y:")),
            span(`class` := "offset-value")(text(model.offsetY.toString))
          ),
          input(
            `type` := "range",
            min := "0",
            max := maxY.max(1).toString,
            value := model.offsetY.min(maxY).toString,
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
            value := currentId.getOrElse(""),
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
    gridConfigs: Option[List[StoredGridConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    selectedGridId: Option[String],
    selectedImageId: Option[String],
    selectedPaletteId: Option[String],
    offsetX: Int,
    offsetY: Int,
    name: String,
    editingId: Option[String],
    prefill: (List[StoredGridConfig], List[StoredImage], List[StoredPalette]) => (Option[String], Option[String], Option[String])
)

enum BuildConfigMsg:
  case LoadedGridConfigs(list: List[StoredGridConfig])
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
  case SaveFailed
  case Back
  case DrawPreview
