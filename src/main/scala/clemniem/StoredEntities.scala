package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Stored entities for galleries: each has id and display name for listing. */

final case class StoredGridConfig(id: String, name: String, config: GridConfig)
object StoredGridConfig {
  given Encoder[StoredGridConfig] = deriveEncoder
  given Decoder[StoredGridConfig] = deriveDecoder
}

final case class StoredPalette(id: String, name: String)
object StoredPalette {
  given Encoder[StoredPalette] = deriveEncoder
  given Decoder[StoredPalette] = deriveDecoder
}

final case class StoredImage(id: String, name: String)
object StoredImage {
  given Encoder[StoredImage] = deriveEncoder
  given Decoder[StoredImage] = deriveDecoder
}

final case class StoredBuildConfig(id: String, name: String, config: BuildConfig)
object StoredBuildConfig {
  given Encoder[StoredBuildConfig] = deriveEncoder
  given Decoder[StoredBuildConfig] = deriveDecoder
}

final case class StoredBuild(id: String, name: String)
object StoredBuild {
  given Encoder[StoredBuild] = deriveEncoder
  given Decoder[StoredBuild] = deriveDecoder
}

/** LocalStorage keys for gallery lists. */
object StorageKeys {
  val gridConfigs  = "gridConfigs"
  val palettes     = "palettes"
  val images       = "images"
  val buildConfigs = "buildConfigs"
  val builds       = "builds"
}
