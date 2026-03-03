package clemniem.screens

import cats.effect.IO
import clemniem.{Screen, ScreenId, ScreenOutput, StorageKeys, StoredBuildConfig, StoredPrintConfig}
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Gallery of saved print configurations. Each entry stores PDF title, colors, step size, etc. */
object PrintGalleryScreen extends Screen {
  type Model = PrintGalleryModel
  type Msg   = PrintGalleryMsg

  val screenId: ScreenId = ScreenId.PrintConfigsId

  def init(previous: Option[Any]): (Model, Cmd[IO, Msg]) = {
    val loadConfigs = Gallery.loadCmd(StorageKeys.printConfigs, PrintGalleryMsg.Loaded.apply, (msg, _) => PrintGalleryMsg.LoadFailed(msg))
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      PrintGalleryMsg.LoadedBuildConfigs.apply,
      (_, _) => PrintGalleryMsg.LoadedBuildConfigs(Nil)
    )
    (PrintGalleryModel(Gallery.initState, None), Cmd.Batch(loadConfigs, loadBuildConfigs))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PrintGalleryMsg.Loaded(list) =>
      (model.copy(gallery = Gallery.onLoaded(model.gallery, list, GalleryLayout.defaultPageSize)), Cmd.None)
    case PrintGalleryMsg.LoadedBuildConfigs(list) =>
      (model.copy(buildConfigs = Some(list)), Cmd.None)
    case PrintGalleryMsg.CreateNew =>
      (model, navCmd(ScreenId.PrintInstructionsId, None))
    case PrintGalleryMsg.Open(stored) =>
      (model, navCmd(ScreenId.PrintInstructionsId, Some(ScreenOutput.OpenPrintConfig(stored))))
    case PrintGalleryMsg.Delete(stored) =>
      (model.copy(gallery = Gallery.onRequestDelete(model.gallery, stored.id)), Cmd.None)
    case PrintGalleryMsg.ConfirmDelete(id) =>
      val (gs, cmd) = Gallery.onConfirmDelete(model.gallery, id, StorageKeys.printConfigs, GalleryLayout.defaultPageSize, PrintGalleryMsg.CancelDelete)
      (model.copy(gallery = gs), cmd)
    case PrintGalleryMsg.CancelDelete =>
      (model.copy(gallery = Gallery.onCancelDelete(model.gallery)), Cmd.None)
    case PrintGalleryMsg.PreviousPage =>
      (model.copy(gallery = Gallery.onPreviousPage(model.gallery)), Cmd.None)
    case PrintGalleryMsg.NextPage =>
      (model.copy(gallery = Gallery.onNextPage(model.gallery, GalleryLayout.defaultPageSize)), Cmd.None)
    case PrintGalleryMsg.Back =>
      (model, navCmd(ScreenId.OverviewId, None))
    case PrintGalleryMsg.LoadFailed(error) =>
      (model.copy(gallery = Gallery.onLoadFailed(model.gallery, error)), Cmd.None)
    case PrintGalleryMsg.ClearData =>
      val cmd = LocalStorageUtils.remove(StorageKeys.printConfigs)(
        _ => PrintGalleryMsg.Retry,
        (_, _) => PrintGalleryMsg.Retry
      )
      (model.copy(gallery = Gallery.initState), cmd)
    case PrintGalleryMsg.Retry =>
      init(None)
  }

  def view(model: Model): Html[Msg] =
    Gallery.view(
      screenId.title,
      model.gallery,
      GalleryLayout.defaultPageSize,
      shortHeader = false,
      PrintGalleryMsg.Back,
      navMsg(ScreenFlow.nextInOverviewOrder(screenId), None),
      PrintGalleryMsg.PreviousPage,
      PrintGalleryMsg.NextPage,
      PrintGalleryMsg.ClearData,
      PrintGalleryMsg.Retry,
      GalleryEmptyState("No print configs saved yet.", "+ Create new", PrintGalleryMsg.CreateNew),
      button(`class` := NesCss.btnPrimary, onClick(PrintGalleryMsg.CreateNew))(text("+ Create new")),
      (item, confirming) => entryCard(item, model.buildConfigs.getOrElse(Nil), confirming)
    )

  private def entryCard(
    item: StoredPrintConfig,
    buildConfigs: List[StoredBuildConfig],
    confirmingDelete: Boolean
  ): Html[Msg] = {
    val buildConfigName = item.selectedBuildConfigId
      .flatMap(id => buildConfigs.find(_.id == id).map(_.name))
      .getOrElse("\u2014")
    val stackedLabel = if (item.stacked) "On" else "Off"
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card gallery-card--start")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(s"Setup: $buildConfigName")),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"Step size: ${item.stepSizePx}px \u00b7 Stacked: $stackedLabel")
        ),
        Gallery.deleteOrActions(
          confirmingDelete,
          item.name,
          item.id,
          PrintGalleryMsg.ConfirmDelete.apply,
          PrintGalleryMsg.CancelDelete,
          button(`class` := NesCss.btnPrimary, onClick(PrintGalleryMsg.Open(item)))(text("Open")),
          button(`class` := NesCss.btnError, onClick(PrintGalleryMsg.Delete(item)))(text("Delete"))
        )
      )
    )
  }
}

final case class PrintGalleryModel(
  gallery: Gallery.State[StoredPrintConfig],
  buildConfigs: Option[List[StoredBuildConfig]]
)

enum PrintGalleryMsg {
  case Loaded(list: List[StoredPrintConfig])
  case LoadFailed(error: String)
  case ClearData
  case Retry
  case LoadedBuildConfigs(list: List[StoredBuildConfig])
  case CreateNew
  case Open(stored: StoredPrintConfig)
  case Delete(stored: StoredPrintConfig)
  case ConfirmDelete(id: String)
  case CancelDelete
  case PreviousPage
  case NextPage
  case Back
}
