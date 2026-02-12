package clemniem.screens

import clemniem.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

/** Shared layout for gallery screens: root container, header (title + back button), and content. */
object GalleryLayout {

  /** Label for back-style buttons (arrow + text). Arrow uses .btn-arrow so it renders at readable size with Press Start 2P. */
  def backButtonLabel[Msg](arrow: String, label: String): Html[Msg] =
    span(span(`class` := "btn-arrow")(text(arrow)), text(" " + label))

  /** Label for next-style buttons (text + arrow). Arrow uses .btn-arrow so it renders at readable size with Press Start 2P. */
  def nextButtonLabel[Msg](label: String, arrow: String): Html[Msg] =
    span(text(label + " "), span(`class` := "btn-arrow")(text(arrow)))

  /** Full back button (← label). Use for screen headers. */
  def backButton[Msg](msg: Msg, label: String): Html[Msg] =
    button(`class` := NesCss.btn, onClick(msg))(backButtonLabel("←", label))

  /** Full next button (Next →). Use for gallery screens that navigate to next in overview order. */
  def nextButton[Msg](msg: Msg): Html[Msg] =
    button(`class` := NesCss.btn, onClick(msg))(nextButtonLabel("Next", "→"))

  /** Total number of pages for a given list size and page size. */
  def totalPagesFor(size: Int, pageSize: Int): Int =
    if (size <= 0) 1 else ((size - 1) / pageSize) + 1

  /** Clamp current page to valid range [1, totalPages]. */
  def clampPage(current: Int, totalPages: Int): Int =
    current.min(totalPages).max(1)

  /** Slice of list for the given page, plus clamped page and total pages. Use with listWithAddActionAndPagination. */
  def sliceForPage[A](list: List[A], currentPage: Int, pageSize: Int): (List[A], Int, Int) = {
    val total = totalPagesFor(list.size, pageSize)
    val page  = clampPage(currentPage, total)
    val start = (page - 1) * pageSize
    val slice = list.slice(start, start + pageSize)
    (slice, page, total)
  }

  /** Paginated gallery list from full list: computes slice and builds list with add action and pagination. */
  def paginatedListWith[A, Msg](
      list: List[A],
      currentPage: Int,
      pageSize: Int,
      addAction: Html[Msg],
      entryCard: A => Html[Msg],
      onPreviousPage: Msg,
      onNextPage: Msg
  ): Html[Msg] = {
    val (slice, page, totalPages) = sliceForPage(list, currentPage, pageSize)
    listWithAddActionAndPagination(addAction, slice.map(entryCard), page, totalPages, onPreviousPage, onNextPage)
  }

  /** Reusable row for gallery card actions (Edit, Delete, etc.). Buttons are styled small via .gallery-actions. */
  def galleryActionsRow[Msg](buttons: Html[Msg]*): Html[Msg] =
    div(`class` := "gallery-actions")(buttons*)

  /** Reusable delete confirmation block: message + Yes + Cancel. Use in gallery entry cards when confirming delete. */
  def galleryDeleteConfirm[Msg](confirmMessage: String, onConfirm: Msg, onCancel: Msg): Html[Msg] =
    div(`class` := "gallery-delete-confirm")(
      span(`class` := "delete-confirm-text nes-text")(text(confirmMessage)),
      button(`class` := NesCss.btnError, onClick(onConfirm))(text("Yes")),
      button(`class` := NesCss.btn, onClick(onCancel))(text("Cancel"))
    )

  /** CSS class for the content area so items don't touch container borders. Use with gallery-list for the list wrapper. */
  val galleryContentClass = "gallery-content"

  /** CSS class for the vertical list of gallery cards + actions (gap between items, no touching). */
  val galleryListClass = "gallery-list"

  /** Default number of entries per page when using pagination. */
  val defaultPageSize: Int = 3

  /** Gallery list with add/create action at the top, then the given entries. Use for all galleries so the add button is consistently first. */
  def listWithAddAction[Msg](addAction: Html[Msg], entries: Iterable[Html[Msg]]): Html[Msg] =
    div(`class` := galleryListClass)((addAction +: entries.toSeq)*)

  /** Paginated gallery list: add action, then a max-height entries area, then Previous / Page x of y / Next. Only shows pagination when totalPages > 1. */
  def listWithAddActionAndPagination[Msg](
      addAction: Html[Msg],
      entriesForCurrentPage: Iterable[Html[Msg]],
      currentPage: Int,
      totalPages: Int,
      onPreviousPage: Msg,
      onNextPage: Msg
  ): Html[Msg] = {
    val paginationBar =
      if (totalPages <= 1) None
      else
        Some(
          div(`class` := "gallery-pagination")(
            button(
              `class` := (if (currentPage <= 1) s"${NesCss.btn} btn-disabled" else NesCss.btn),
              onClick(onPreviousPage)
            )(backButtonLabel("←", "Previous")),
            span(`class` := "gallery-pagination-label nes-text")(
              text(s"Page $currentPage of $totalPages")
            ),
            button(
              `class` := (if (currentPage >= totalPages) s"${NesCss.btn} btn-disabled" else NesCss.btn),
              onClick(onNextPage)
            )(nextButtonLabel("Next", "→"))
          )
        )
    val children: Seq[Html[Msg]] =
      addAction +: div(`class` := "gallery-list-entries")(entriesForCurrentPage.toSeq*) +: paginationBar.toList
    div(`class` := galleryListClass)(children*)
  }

  /** @param title       Screen title (e.g. "Palettes", "Build configs")
    * @param backButton  Back button HTML (e.g. ← Overview)
    * @param content     Main content: loading state, empty state, or list + actions (will be wrapped in gallery-content)
    * @param shortHeader If true, use smaller margin below header
    * @param nextButton  Optional "Next →" button (navigates to next screen in overview order)
    */
  def apply[Msg](
      title: String,
      backButton: Html[Msg],
      content: Html[Msg],
      shortHeader: Boolean,
      nextButton: Option[Html[Msg]]
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    val headerButtons = nextButton match {
      case Some(next) => div(`class` := "flex-row", style := "gap: 0.5rem;")(backButton, next)
      case None       => backButton
    }
    div(`class` := s"${NesCss.container} ${NesCss.containerRounded} screen-container screen-container--gameboy")(
      div(`class` := headerClass)(
        h1(`class` := "screen-title")(text(title)),
        headerButtons
      ),
      div(`class` := s"$galleryContentClass screen-container-inner")(content)
    )
  }
}
