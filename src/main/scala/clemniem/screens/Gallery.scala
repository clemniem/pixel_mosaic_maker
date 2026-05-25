package clemniem.screens

import cats.effect.IO
import clemniem.StoredEntity
import clemniem.common.{Loadable, LocalStorageUtils}
import clemniem.common.nescss.NesCss
import io.circe.{Decoder, Encoder}
import tyrian.Html.*
import tyrian.*

/** Reusable gallery state and update/view helpers.
  *
  * Each gallery screen composes this with its own model and messages:
  *   - Embed [[State]] in the screen model (simple galleries can use it directly)
  *   - Call the update helpers from the screen's update function
  *   - Call [[view]] from the screen's view function
  */
object Gallery {

  /** Common gallery model fields: paginated list of entities with optional pending-delete. */
  final case class State[A](
    items: Loadable[List[A]],
    pendingDeleteId: Option[String],
    currentPage: Int)

  /** Initial empty gallery state (loading). */
  def initState[A]: State[A] = State(Loadable.Loading, None, 1)

  /** Create a [[Cmd]] that loads a list from LocalStorage. */
  def loadCmd[A, M](
    storageKey: String,
    onSuccess: List[A] => M,
    onFailure: (String, String) => M
  )(using Decoder[List[A]]
  ): Cmd[IO, M] =
    LocalStorageUtils.loadList(storageKey)(onSuccess, onFailure)

  // ---------------------------------------------------------------------------
  // Update helpers
  // ---------------------------------------------------------------------------

  /** Handle entity list loaded: store list and clamp page. */
  def onLoaded[A](state: State[A], list: List[A], pageSize: Int): State[A] = {
    val totalPages = GalleryLayout.totalPagesFor(list.size, pageSize)
    state.copy(items = Loadable.Loaded(list), currentPage = GalleryLayout.clampPage(state.currentPage, totalPages))
  }

  /** Handle load failure: store the error message. */
  def onLoadFailed[A](state: State[A], error: String): State[A] =
    state.copy(items = Loadable.Failed(error))

  /** Handle delete request: set pendingDeleteId. */
  def onRequestDelete[A](state: State[A], id: String): State[A] =
    state.copy(pendingDeleteId = Some(id))

  /** Handle confirm delete: filter list, save, and clamp page. Returns (updatedState, saveCmd). */
  def onConfirmDelete[A <: StoredEntity, M](
    state: State[A],
    id: String,
    storageKey: String,
    pageSize: Int,
    cancelMsg: M
  )(using Encoder[List[A]]
  ): (State[A], Cmd[IO, M]) = {
    val listOpt                    = state.items.toOption
    val (newListOpt, newPage, cmd) = LocalStorageUtils.confirmDelete(
      listOpt,
      id,
      storageKey,
      pageSize,
      state.currentPage,
      cancelMsg,
      _.id
    )
    val newItems = newListOpt match {
      case Some(list) => Loadable.Loaded(list)
      case None       => state.items
    }
    (state.copy(items = newItems, pendingDeleteId = None, currentPage = newPage), cmd)
  }

  /** Handle cancel delete: clear pendingDeleteId. */
  def onCancelDelete[A](state: State[A]): State[A] =
    state.copy(pendingDeleteId = None)

  /** Handle previous page. */
  def onPreviousPage[A](state: State[A]): State[A] =
    state.copy(currentPage = (state.currentPage - 1).max(1))

  /** Handle next page. */
  def onNextPage[A](state: State[A], pageSize: Int): State[A] = {
    val totalPages = state.items.toOption.map(l => GalleryLayout.totalPagesFor(l.size, pageSize)).getOrElse(1)
    state.copy(currentPage = (state.currentPage + 1).min(totalPages))
  }

  // ---------------------------------------------------------------------------
  // View helpers
  // ---------------------------------------------------------------------------

  /** Standard gallery view with loading/empty/failed/list branching.
    *
    * @param title
    *   Screen title
    * @param state
    *   Gallery state
    * @param pageSize
    *   Entries per page
    * @param shortHeader
    *   If true, use compact header spacing
    * @param backMsg
    *   Message for the back button
    * @param nextMsg
    *   Message for the "Next" button
    * @param prevPageMsg
    *   Message for pagination previous
    * @param nextPageMsg
    *   Message for pagination next
    * @param clearDataMsg
    *   Message emitted when user clicks "Clear data" on a failed load
    * @param retryMsg
    *   Message emitted when user clicks "Retry" on a failed load
    * @param emptyContent
    *   Content to show when list is empty
    * @param addAction
    *   Action button/row shown above the list (e.g. "+ New layout")
    * @param entryCard
    *   Render a single entry card; receives (entity, isConfirmingDelete)
    */
  def view[A <: StoredEntity, M](
    title: String,
    state: State[A],
    pageSize: Int,
    shortHeader: Boolean,
    backMsg: M,
    nextMsg: M,
    prevPageMsg: M,
    nextPageMsg: M,
    clearDataMsg: M,
    retryMsg: M,
    emptyContent: Html[M],
    addAction: Html[M],
    entryCard: (A, Boolean) => Html[M]
  ): Html[M] = {
    val backBtn = GalleryLayout.backButton(backMsg, "Overview")
    val nextBtn = GalleryLayout.nextButton(nextMsg)
    state.items match {
      case Loadable.Loading =>
        GalleryLayout(title, backBtn, p(`class` := NesCss.text)(text("Loading\u2026")), shortHeader, Some(nextBtn))
      case Loadable.Failed(error) =>
        GalleryLayout(title, backBtn, failedView(error, clearDataMsg, retryMsg), shortHeader, Some(nextBtn))
      case Loadable.Loaded(list) =>
        val content =
          if (list.isEmpty) emptyContent
          else
            GalleryLayout.paginatedListWith(
              list,
              state.currentPage,
              pageSize,
              addAction,
              item => entryCard(item, state.pendingDeleteId.contains(item.id)).setKey(item.id),
              prevPageMsg,
              nextPageMsg
            )
        GalleryLayout(title, backBtn, content, shortHeader, Some(nextBtn))
    }
  }

  /** Error UI for a failed load: shows the error message with Clear data and Retry buttons. */
  def failedView[M](error: String, clearDataMsg: M, retryMsg: M): Html[M] =
    div(`class` := s"${NesCss.container} empty-state")(
      p(`class` := NesCss.text)(text("Could not load data.")),
      p(`class` := "nes-text", style := "font-size: 0.75rem; word-break: break-all;")(text(error)),
      div(`class` := "flex-row", style := "margin-top: 0.75rem;")(
        button(`class` := NesCss.btnError, onClick(clearDataMsg))(text("Clear data")),
        button(`class` := NesCss.btn, onClick(retryMsg))(text("Retry"))
      )
    )

  /** Delete confirmation or action buttons for an entry card. */
  def deleteOrActions[M](
    confirmingDelete: Boolean,
    entityName: String,
    entityId: String,
    confirmDeleteMsg: String => M,
    cancelDeleteMsg: M,
    actionButtons: Html[M]*
  ): Html[M] =
    if (confirmingDelete)
      GalleryLayout.galleryDeleteConfirm(
        s"Delete \"$entityName\"?",
        confirmDeleteMsg(entityId),
        cancelDeleteMsg
      )
    else
      GalleryLayout.galleryActionsRow(actionButtons*)
}
