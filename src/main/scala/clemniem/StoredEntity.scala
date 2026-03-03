package clemniem

/** Common trait for all stored entities: each has an id and a display name. */
trait StoredEntity {
  def id: String
  def name: String
}
