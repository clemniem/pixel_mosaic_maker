package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Stored entities for galleries: each has id and display name for listing. */

final case class StoredGridConfig(
    id: String,
    name: String,
    config: GridConfig,
    mode: Option[GridDefMode] = None,
    rowDefs: Option[List[RowDef]] = None,
    columnDefs: Option[List[ColumnDef]] = None
)
object StoredGridConfig {
  given Encoder[StoredGridConfig] = deriveEncoder
  given Decoder[StoredGridConfig] = deriveDecoder
}

/** Default palette when loading old saves that had no colors field. */
private val defaultPaletteColors: Vector[Color] =
  Vector(Color(0, 0, 0), Color(255, 255, 255), Color(200, 50, 50), Color(50, 120, 200))

final case class StoredPalette(id: String, name: String, colors: Vector[Color])
object StoredPalette {
  given Encoder[StoredPalette] = deriveEncoder
  private def storedPaletteDecoder: Decoder[StoredPalette] = {
    val withColors    = io.circe.Decoder.forProduct3("id", "name", "colors")(StoredPalette.apply)
    val withoutColors = io.circe.Decoder.forProduct2("id", "name")((id: String, name: String) => StoredPalette(id, name, defaultPaletteColors))
    withColors.or(withoutColors)
  }
  given Decoder[StoredPalette] = storedPaletteDecoder
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
