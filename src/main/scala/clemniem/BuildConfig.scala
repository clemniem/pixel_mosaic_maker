package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Stateless build definition: Palette + Layout + Image + Offset. */
final case class BuildConfig(
    grid: Layout,
    imageRef: String,
    paletteRef: String,
    offsetX: Int,
    offsetY: Int
)

object BuildConfig {
  given Encoder[BuildConfig] = deriveEncoder
  given Decoder[BuildConfig] = deriveDecoder
}
