package clemniem.common

/** ADT for data that is loaded asynchronously. Replaces the `Option[List[X]]` pattern where
  * `None` = loading, `Some(Nil)` = empty, `Some(list)` = loaded, and decode errors silently map to empty.
  */
enum Loadable[+A] {
  case Loading
  case Loaded(value: A)
  case Failed(error: String)

  def toOption: Option[A] = this match {
    case Loaded(v) => Some(v)
    case _         => None
  }

  def isLoaded: Boolean = this match {
    case Loaded(_) => true
    case _         => false
  }

  def map[B](f: A => B): Loadable[B] = this match {
    case Loading    => Loading
    case Loaded(v)  => Loaded(f(v))
    case Failed(e)  => Failed(e)
  }

  def getOrElse[B >: A](fallback: => B): B = this match {
    case Loaded(v) => v
    case _         => fallback
  }
}
