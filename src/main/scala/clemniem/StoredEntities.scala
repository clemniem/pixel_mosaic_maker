package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Common trait for all stored entities: each has an id and a display name. */
trait StoredEntity {
  def id: String
  def name: String
}

/** Stored entities for galleries: each has id and display name for listing. */

final case class StoredLayout(
  id: String,
  name: String,
  config: Layout,
  mode: Option[GridDefMode] = None,
  rowDefs: Option[List[RowDef]] = None,
  columnDefs: Option[List[ColumnDef]] = None) extends StoredEntity
object StoredLayout {
  given Encoder[StoredLayout] = deriveEncoder
  given Decoder[StoredLayout] = deriveDecoder
}

final case class StoredPalette(id: String, name: String, colors: Vector[Color]) extends StoredEntity
object StoredPalette {
  given Encoder[StoredPalette] = deriveEncoder
  given Decoder[StoredPalette] = deriveDecoder
}

final case class StoredImage(id: String, name: String, pixelPic: PixelPic) extends StoredEntity
object StoredImage {
  given Encoder[StoredImage] = deriveEncoder
  given Decoder[StoredImage] = deriveDecoder
}

final case class StoredBuildConfig(id: String, name: String, config: BuildConfig, savedStepIndex: Option[Int] = None) extends StoredEntity
object StoredBuildConfig {
  given Encoder[StoredBuildConfig] = deriveEncoder
  given Decoder[StoredBuildConfig] = deriveDecoder
}

/** A build in progress: references a build config and stores saved step and patch background color. */
final case class StoredBuild(
  id: String,
  name: String,
  buildConfigRef: String,
  savedStepIndex: Option[Int] = None,
  patchBackgroundColorHex: Option[String] = None) extends StoredEntity
object StoredBuild {
  given Encoder[StoredBuild] = deriveEncoder
  given Decoder[StoredBuild] = deriveDecoder
}

final case class StoredPrintConfig(
  id: String,
  name: String,
  selectedBuildId: Option[String],
  selectedBuildConfigId: Option[String],
  title: String,
  stepSizePx: Int,
  pageBackgroundColorHex: String,
  patchBackgroundColorHex: String,
  stacked: Boolean,
  printerMarginMm: Double,
  contentTopOffsetMm: Double
) extends StoredEntity
object StoredPrintConfig {
  given Encoder[StoredPrintConfig] = deriveEncoder
  given Decoder[StoredPrintConfig] = deriveDecoder
}

/** LocalStorage keys for gallery lists. */
object StorageKeys {
  val layouts      = "gridConfigs"
  val palettes     = "palettes"
  val images       = "images"
  val buildConfigs = "buildConfigs"
  val builds       = "builds"
  val printConfigs = "printConfigs"
}
