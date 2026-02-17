package clemniem.common

import cats.effect.IO
import clemniem.screens.GalleryLayout
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import tyrian.Cmd
import tyrian.cmds.LocalStorage

import scala.annotation.unused

/** Generic LocalStorage helpers to save/load case classes (JSON via circe). Use these to persist multiple outputs per
  * type (e.g. 3 grid configs, 3 palettes, 3 images).
  *
  * Keying strategies:
  *   - One item per key: e.g. `"layout/abc-123"` → single [[Layout]]; use [[save]]/[[load]].
  *   - List under one key: e.g. `"gridConfigs"` → `List[StoredLayout]`; use [[saveList]]/[[loadList]]. (Key string kept
  *     for backward compatibility with existing LocalStorage data.)
  */
object LocalStorageUtils {

  /** Save a single item at `key`. Emits `success(item)` on success, `failure(message, item)` on error. */
  def save[A, M](
    key: String,
    item: A
  )(success: A => M,
    failure: (String, A) => M
  )(using Encoder[A]
  ): Cmd[IO, M] = {
    val json = item.asJson.noSpacesSortKeys
    LocalStorage.setItem(key, json) {
      case LocalStorage.Result.Success          => success(item)
      case LocalStorage.Result.Failure(message) => failure(message, item)
      case e                                    => failure(e.toString, item)
    }
  }

  /** Load a single item from `key`. Emits `success(a)` when decoded, `notFound(key)` when missing, `failure(msg, key)`
    * on decode error.
    */
  def load[A, M](key: String)(success: A => M, notFound: String => M, failure: (String, String) => M)(using Decoder[A])
    : Cmd[IO, M] =
    LocalStorage.getItem(key) {
      case Left(_) =>
        notFound(key)
      case Right(result) =>
        result match {
          case found: LocalStorage.Result.Found =>
            io.circe.parser.decode[A](found.data) match {
              case Left(decodeErr) => failure(decodeErr.getMessage, key)
              case Right(decoded)  => success(decoded)
            }
        }
    }

  /** Save a list at `key`. Use for "all grid configs", "all palettes", etc. */
  def saveList[A, M](
    key: String,
    items: List[A]
  )(success: List[A] => M,
    failure: (String, List[A]) => M
  )(using Encoder[List[A]]
  ): Cmd[IO, M] = {
    val json = items.asJson.noSpacesSortKeys
    LocalStorage.setItem(key, json) {
      case LocalStorage.Result.Success          => success(items)
      case LocalStorage.Result.Failure(message) => failure(message, items)
      case e                                    => failure(e.toString, items)
    }
  }

  /** Load a list from `key`. Emits empty list when key is missing (so you can start with Nil). */
  def loadList[A, M](
    key: String
  )(success: List[A] => M,
    @unused notFound: String => M,
    failure: (String, String) => M
  )(using Decoder[List[A]]
  ): Cmd[IO, M] =
    LocalStorage.getItem(key) {
      case Left(_) =>
        success(Nil)
      case Right(result) =>
        result match {
          case found: LocalStorage.Result.Found =>
            io.circe.parser.decode[List[A]](found.data) match {
              case Left(decodeErr) => failure(decodeErr.getMessage, key)
              case Right(list)     => success(list)
            }
        }
    }

  /** Handle a gallery confirm-delete: filter the list, save it, and compute the clamped page. Returns
    * `(newListOpt, newPage, saveCmd)` — the caller just destructures and updates its model fields.
    */
  def confirmDelete[A, M](
    listOpt: Option[List[A]],
    id: String,
    storageKey: String,
    pageSize: Int,
    currentPage: Int,
    cancelMsg: M,
    getId: A => String
  )(using Encoder[List[A]]
  ): (Option[List[A]], Int, Cmd[IO, M]) =
    listOpt match {
      case Some(list) =>
        val newList    = list.filterNot(a => getId(a) == id)
        val saveCmd    = saveList(storageKey, newList)(_ => cancelMsg, (_, _) => cancelMsg)
        val totalPages = GalleryLayout.totalPagesFor(newList.size, pageSize)
        (Some(newList), GalleryLayout.clampPage(currentPage, totalPages), saveCmd)
      case None =>
        (listOpt, currentPage, Cmd.None)
    }

  /** Remove one key. Emits `success(key)` or `failure(msg, key)`. */
  def remove[M](key: String)(success: String => M, failure: (String, String) => M): Cmd[IO, M] =
    LocalStorage.removeItem(key) {
      case LocalStorage.Result.Success          => success(key)
      case LocalStorage.Result.Failure(message) => failure(message, key)
      case e                                    => failure(e.toString, key)
    }
}
