package clemniem.screens

import cats.effect.IO
import clemniem.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredBuildConfig, StoredPrintConfig}
import clemniem.common.LocalStorageUtils
import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Gallery of saved print configurations. Each entry stores PDF title, colors, step size, etc. */
object PrintGalleryScreen extends Screen {
  type Model = PrintGalleryModel
  type Msg   = PrintGalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.PrintConfigsId

  def init(previous: Option[clemniem.ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val loadConfigs = LocalStorageUtils.loadList(StorageKeys.printConfigs)(
      PrintGalleryMsg.Loaded.apply,
      _ => PrintGalleryMsg.Loaded(Nil),
      (_, _) => PrintGalleryMsg.Loaded(Nil)
    )
    val loadBuildConfigs = LocalStorageUtils.loadList(StorageKeys.buildConfigs)(
      PrintGalleryMsg.LoadedBuildConfigs.apply,
      _ => PrintGalleryMsg.LoadedBuildConfigs(Nil),
      (_, _) => PrintGalleryMsg.LoadedBuildConfigs(Nil)
    )
    (PrintGalleryModel(None, None, pendingDeleteId = None, currentPage = 1), Cmd.Batch(loadConfigs, loadBuildConfigs))
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case PrintGalleryMsg.Loaded(list) =>
      val totalPages = GalleryLayout.totalPagesFor(list.size, GalleryLayout.defaultPageSize)
      (model.copy(list = Some(list), currentPage = GalleryLayout.clampPage(model.currentPage, totalPages)), Cmd.None)
    case PrintGalleryMsg.LoadedBuildConfigs(list) =>
      (model.copy(buildConfigs = Some(list)), Cmd.None)
    case PrintGalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.PrintInstructionsId, None)))
    case PrintGalleryMsg.Open(stored) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.PrintInstructionsId, Some(ScreenOutput.OpenPrintConfig(stored)))))
    case PrintGalleryMsg.Delete(stored) =>
      (model.copy(pendingDeleteId = Some(stored.id)), Cmd.None)
    case PrintGalleryMsg.ConfirmDelete(id) =>
      val (newList, newPage, cmd) = LocalStorageUtils.confirmDelete(
        model.list,
        id,
        StorageKeys.printConfigs,
        GalleryLayout.defaultPageSize,
        model.currentPage,
        PrintGalleryMsg.CancelDelete,
        _.id
      )
      (model.copy(list = newList, pendingDeleteId = None, currentPage = newPage), cmd)
    case PrintGalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)
    case PrintGalleryMsg.PreviousPage =>
      (model.copy(currentPage = (model.currentPage - 1).max(1)), Cmd.None)
    case PrintGalleryMsg.NextPage =>
      val totalPages = GalleryLayout.totalPagesFor(model.list.map(_.size).getOrElse(0), GalleryLayout.defaultPageSize)
      (model.copy(currentPage = (model.currentPage + 1).min(totalPages)), Cmd.None)
    case PrintGalleryMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.OverviewId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val backBtn = GalleryLayout.backButton(PrintGalleryMsg.Back, "Overview")
    val nextBtn = GalleryLayout.nextButton(NavigateNext(ScreenId.nextInOverviewOrder(screenId), None))
    model.list match {
      case None =>
        GalleryLayout(screenId.title, backBtn, p(`class` := NesCss.text)(text("Loading…")), shortHeader = false, Some(nextBtn))
      case Some(list) =>
        val createBtn = button(`class` := NesCss.btnPrimary, onClick(PrintGalleryMsg.CreateNew))(text("+ Create new"))
        val content =
          if (list.isEmpty)
            GalleryEmptyState("No print configs saved yet.", "+ Create new", PrintGalleryMsg.CreateNew)
          else
            GalleryLayout.paginatedListWith(
              list,
              model.currentPage,
              GalleryLayout.defaultPageSize,
              createBtn,
              item => entryCard(item, model.buildConfigs.getOrElse(Nil), model.pendingDeleteId.contains(item.id)),
              PrintGalleryMsg.PreviousPage,
              PrintGalleryMsg.NextPage
            )
        GalleryLayout(screenId.title, backBtn, content, shortHeader = false, Some(nextBtn))
    }
  }

  private def entryCard(
    item: StoredPrintConfig,
    buildConfigs: List[StoredBuildConfig],
    confirmingDelete: Boolean
  ): Html[Msg] = {
    val buildConfigName = item.selectedBuildConfigId
      .flatMap(id => buildConfigs.find(_.id == id).map(_.name))
      .getOrElse("—")
    val stackedLabel = if (item.stacked) "On" else "Off"
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} gallery-card gallery-card--start")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(item.name)),
        span(`class` := "gallery-card-meta nes-text")(text(s"Setup: $buildConfigName")),
        span(`class` := "gallery-card-meta nes-text")(
          text(s"Step size: ${item.stepSizePx}px · Stacked: $stackedLabel")
        ),
        if (confirmingDelete)
          GalleryLayout.galleryDeleteConfirm(
            s"Delete \"${item.name}\"?",
            PrintGalleryMsg.ConfirmDelete(item.id),
            PrintGalleryMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
            button(`class` := NesCss.btnPrimary, onClick(PrintGalleryMsg.Open(item)))(text("Open")),
            button(`class` := NesCss.btnError, onClick(PrintGalleryMsg.Delete(item)))(text("Delete"))
          )
      )
    )
  }
}

final case class PrintGalleryModel(
  list: Option[List[StoredPrintConfig]],
  buildConfigs: Option[List[StoredBuildConfig]],
  pendingDeleteId: Option[String],
  currentPage: Int
)

enum PrintGalleryMsg {
  case Loaded(list: List[StoredPrintConfig])
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
