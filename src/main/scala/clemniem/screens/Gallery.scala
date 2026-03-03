package clemniem.screens

import cats.effect.IO
import clemniem.StoredEntity
import clemniem.common.LocalStorageUtils
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
    items: Option[List[A]],
    pendingDeleteId: Option[String],
    currentPage: Int
  )

  /** Initial empty gallery state (loading). */
  def initState[A]: State[A] = State(None, None, 1)

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
    state.copy(items = Some(list), currentPage = GalleryLayout.clampPage(state.currentPage, totalPages))
  }

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
    val (newList, newPage, cmd) = LocalStorageUtils.confirmDelete(
      state.items,
      id,
      storageKey,
      pageSize,
      state.currentPage,
      cancelMsg,
      _.id
    )
    (state.copy(items = newList, pendingDeleteId = None, currentPage = newPage), cmd)
  }

  /** Handle cancel delete: clear pendingDeleteId. */
  def onCancelDelete[A](state: State[A]): State[A] =
    state.copy(pendingDeleteId = None)

  /** Handle previous page. */
  def onPreviousPage[A](state: State[A]): State[A] =
    state.copy(currentPage = (state.currentPage - 1).max(1))

  /** Handle next page. */
  def onNextPage[A](state: State[A], pageSize: Int): State[A] = {
    val totalPages = state.items.map(l => GalleryLayout.totalPagesFor(l.size, pageSize)).getOrElse(1)
    state.copy(currentPage = (state.currentPage + 1).min(totalPages))
  }

  // ---------------------------------------------------------------------------
  // View helper
  // ---------------------------------------------------------------------------

  /** Standard gallery view with loading/empty/list branching.
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
    emptyContent: Html[M],
    addAction: Html[M],
    entryCard: (A, Boolean) => Html[M]
  ): Html[M] = {
    val backBtn = GalleryLayout.backButton(backMsg, "Overview")
    val nextBtn = GalleryLayout.nextButton(nextMsg)
    state.items match {
      case None =>
        GalleryLayout(title, backBtn, p(`class` := NesCss.text)(text("Loading\u2026")), shortHeader, Some(nextBtn))
      case Some(list) =>
        val content =
          if (list.isEmpty) emptyContent
          else
            GalleryLayout.paginatedListWith(
              list,
              state.currentPage,
              pageSize,
              addAction,
              item => entryCard(item, state.pendingDeleteId.contains(item.id)),
              prevPageMsg,
              nextPageMsg
            )
        GalleryLayout(title, backBtn, content, shortHeader, Some(nextBtn))
    }
  }

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
