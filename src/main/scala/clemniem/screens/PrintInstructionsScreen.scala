package clemniem.screens

import cats.effect.IO
import clemniem.{
  GridConfig,
  NavigateNext,
  PixelPic,
  Screen,
  ScreenId,
  StoredBuildConfig,
  StoredImage,
  StoredPalette
}
import clemniem.common.{LocalStorageUtils, PdfUtils, PrintBookRequest}
import clemniem.StorageKeys
import tyrian.Html.*
import tyrian.*

/** Screen to generate the PDF print instructions: choose a BuildConfig and set options (e.g. title). */
object PrintInstructionsScreen extends Screen {
  type Model = PrintInstructionsModel
  type Msg   = PrintInstructionsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.PrintInstructionsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val model = PrintInstructionsModel(
      buildConfigs = None,
      images = None,
      palettes = None,
      selectedBuildConfigId = None,
      title = "Mosaic"
    )
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      PrintInstructionsMsg.LoadedBuildConfigs.apply,
      _ => PrintInstructionsMsg.LoadedBuildConfigs(Nil),
      (_, _) => PrintInstructionsMsg.LoadedBuildConfigs(Nil)
    )
    val loadImages = LocalStorageUtils.loadList(StorageKeys.images)(
      PrintInstructionsMsg.LoadedImages.apply,
      _ => PrintInstructionsMsg.LoadedImages(Nil),
      (_, _) => PrintInstructionsMsg.LoadedImages(Nil)
    )
    val loadPalettes = LocalStorageUtils.loadList(StorageKeys.palettes)(
      PrintInstructionsMsg.LoadedPalettes.apply,
      _ => PrintInstructionsMsg.LoadedPalettes(Nil),
      (_, _) => PrintInstructionsMsg.LoadedPalettes(Nil)
    )
    (model, Cmd.Batch(loadBuildConfigs, loadImages, loadPalettes))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PrintInstructionsMsg.LoadedBuildConfigs(list) =>
      val selectedId = model.selectedBuildConfigId.orElse(list.headOption.map(_.id))
      (model.copy(buildConfigs = Some(list), selectedBuildConfigId = selectedId), Cmd.None)
    case PrintInstructionsMsg.LoadedImages(list) =>
      (model.copy(images = Some(list)), Cmd.None)
    case PrintInstructionsMsg.LoadedPalettes(list) =>
      (model.copy(palettes = Some(list)), Cmd.None)
    case PrintInstructionsMsg.SetBuildConfig(id) =>
      (model.copy(selectedBuildConfigId = Some(id)), Cmd.None)
    case PrintInstructionsMsg.SetTitle(title) =>
      (model.copy(title = title), Cmd.None)
    case PrintInstructionsMsg.PrintPdf =>
      val request = PrintBookRequest(
        title = if (model.title.trim.nonEmpty) model.title.trim else "Mosaic",
        mosaicPicAndGridOpt = model.selectedStored.flatMap(stored =>
          mosaicPicAndGridForStored(stored, model.images.getOrElse(Nil), model.palettes.getOrElse(Nil))
        )
      )
      (model, Cmd.SideEffect(PdfUtils.printBookPdf(request)))
    case PrintInstructionsMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val container = "font-family: system-ui, sans-serif; max-width: 36rem; margin: 0 auto; padding: 1.5rem;"
    val buildConfigs = model.buildConfigs.getOrElse(Nil)
    val selectedId   = model.selectedBuildConfigId.orElse(buildConfigs.headOption.map(_.id))
    val canPrint     = model.selectedStored.isDefined

    div(style := container)(
      div(style := "display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem; flex-wrap: wrap; gap: 8px;")(
        h2(style := "margin: 0;")(text("Print Instructions")),
        button(
          style := "padding: 6px 12px; cursor: pointer; border: 1px solid #555; border-radius: 4px; background: #fff;",
          onClick(PrintInstructionsMsg.Back)
        )(text("← Overview"))
      ),
      p(style := "color: #555; margin-bottom: 1.5rem;")(
        text("Choose a build config and set the booklet title, then generate the PDF.")
      ),
      div(style := "margin-bottom: 1rem;")(
        label(style := "display: block; font-weight: 500; margin-bottom: 0.35rem;")(text("Build config")),
        model.buildConfigs match {
          case None =>
            span(style := "color: #666;")(text("Loading…"))
          case Some(list) if list.isEmpty =>
            span(style := "color: #666;")(text("No build configs saved. Create one from the BuildConfigs gallery."))
          case Some(list) =>
            select(
              style := "padding: 6px 10px; min-width: 16rem; border: 1px solid #ccc; border-radius: 4px;",
              value := selectedId.getOrElse(""),
              onInput(s => PrintInstructionsMsg.SetBuildConfig(if (s.isEmpty) list.headOption.map(_.id).getOrElse("") else s))
            )(
              list.map { item =>
                option(value := item.id)(text(item.name))
              }*
            )
        }
      ),
      div(style := "margin-bottom: 1.5rem;")(
        label(style := "display: block; font-weight: 500; margin-bottom: 0.35rem;")(text("Title")),
        input(
          `type` := "text",
          value := model.title,
          onInput(PrintInstructionsMsg.SetTitle.apply),
          style := "padding: 6px 10px; width: 100%; max-width: 20rem; border: 1px solid #ccc; border-radius: 4px; box-sizing: border-box;"
        )
      ),
      button(
        style := (if (!canPrint)
          "padding: 8px 16px; cursor: not-allowed; opacity: 0.6; background: #999; color: #fff; border: none; border-radius: 4px; font-weight: 500;"
        else
          "padding: 8px 16px; cursor: pointer; background: #1565c0; color: #fff; border: none; border-radius: 4px; font-weight: 500;"),
        onClick(PrintInstructionsMsg.PrintPdf)
      )(text("Print PDF"))
    )
  }

  private def mosaicPicAndGridForStored(
      stored: StoredBuildConfig,
      images: List[StoredImage],
      palettes: List[StoredPalette]
  ): Option[(PixelPic, GridConfig)] =
    for {
      img     <- images.find(_.id == stored.config.imageRef)
      palette <- palettes.find(_.id == stored.config.paletteRef)
      pic     = clemniem.PaletteUtils.applyPaletteToPixelPic(img.pixelPic, palette)
      gw      = stored.config.grid.width
      gh      = stored.config.grid.height
      cropped <- pic.crop(stored.config.offsetX, stored.config.offsetY, gw, gh)
    } yield (cropped, stored.config.grid)
}

final case class PrintInstructionsModel(
    buildConfigs: Option[List[StoredBuildConfig]],
    images: Option[List[StoredImage]],
    palettes: Option[List[StoredPalette]],
    selectedBuildConfigId: Option[String],
    title: String
) {
  def selectedStored: Option[StoredBuildConfig] =
    buildConfigs.flatMap(list =>
      selectedBuildConfigId.flatMap(id => list.find(_.id == id))
    )
}

enum PrintInstructionsMsg:
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case LoadedImages(list: List[StoredImage])
  case LoadedPalettes(list: List[StoredPalette])
  case SetBuildConfig(id: String)
  case SetTitle(title: String)
  case PrintPdf
  case Back
