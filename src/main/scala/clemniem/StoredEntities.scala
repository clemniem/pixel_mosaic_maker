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

/** Placeholder for old saves that had no pixelPic. */
private def defaultPixelPic(name: String): PixelPic =
  PixelPic(1, 1, Vector(Pixel(0, 0, 0, 255)), Vector(0), Map(0 -> 1), name).get

final case class StoredImage(id: String, name: String, pixelPic: PixelPic)
object StoredImage {
  given Encoder[StoredImage] = deriveEncoder
  private def storedImageDecoder: Decoder[StoredImage] =
    io.circe.Decoder.forProduct3("id", "name", "pixelPic")(StoredImage.apply)
      .or(io.circe.Decoder.forProduct2("id", "name")((id: String, name: String) => StoredImage(id, name, defaultPixelPic(name))))
  given Decoder[StoredImage] = storedImageDecoder
}

final case class StoredBuildConfig(id: String, name: String, config: BuildConfig, savedStepIndex: Option[Int] = None)
object StoredBuildConfig {
  given Encoder[StoredBuildConfig] = deriveEncoder
  private def storedBuildConfigDecoder: Decoder[StoredBuildConfig] = {
    val withStep = io.circe.Decoder.forProduct4("id", "name", "config", "savedStepIndex")(StoredBuildConfig.apply)
    val withoutStep = io.circe.Decoder.forProduct3("id", "name", "config")((id: String, name: String, config: BuildConfig) => StoredBuildConfig(id, name, config, None))
    withStep.or(withoutStep)
  }
  given Decoder[StoredBuildConfig] = storedBuildConfigDecoder
}

/** A build in progress: references a build config and stores saved step. */
final case class StoredBuild(id: String, name: String, buildConfigRef: String, savedStepIndex: Option[Int] = None)
object StoredBuild {
  given Encoder[StoredBuild] = deriveEncoder
  private def storedBuildDecoder: Decoder[StoredBuild] = {
    val withRef = io.circe.Decoder.forProduct4("id", "name", "buildConfigRef", "savedStepIndex")(StoredBuild.apply)
    val withoutRef = io.circe.Decoder.forProduct2("id", "name")((id: String, name: String) => StoredBuild(id, name, "", None))
    withRef.or(withoutRef)
  }
  given Decoder[StoredBuild] = storedBuildDecoder
}

/** LocalStorage keys for gallery lists. */
object StorageKeys {
  val gridConfigs  = "gridConfigs"
  val palettes     = "palettes"
  val images       = "images"
  val buildConfigs = "buildConfigs"
  val builds       = "builds"
}
