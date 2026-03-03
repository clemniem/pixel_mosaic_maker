package clemniem.common

/** Pure pagination helpers. Used by galleries and LocalStorageUtils (no UI dependency). */
object Pagination {

  /** Total number of pages for a given list size and page size. */
  def totalPagesFor(size: Int, pageSize: Int): Int =
    if (size <= 0) 1 else ((size - 1) / pageSize) + 1

  /** Clamp current page to valid range [1, totalPages]. */
  def clampPage(current: Int, totalPages: Int): Int =
    current.min(totalPages).max(1)

  /** Slice of list for the given page, plus clamped page and total pages. */
  def sliceForPage[A](list: List[A], currentPage: Int, pageSize: Int): (List[A], Int, Int) = {
    val total = totalPagesFor(list.size, pageSize)
    val page  = clampPage(currentPage, total)
    val start = (page - 1) * pageSize
    val slice = list.slice(start, start + pageSize)
    (slice, page, total)
  }
}
