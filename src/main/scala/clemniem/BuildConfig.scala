package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Stateless build definition: Palette + GridConfig + Image + Offset. */
final case class BuildConfig(
    grid: GridConfig,
    imageRef: String,
    paletteRef: String,
    offsetX: Int,
    offsetY: Int
)

object BuildConfig {
  given Encoder[BuildConfig] = deriveEncoder
  given Decoder[BuildConfig] = deriveDecoder
}
