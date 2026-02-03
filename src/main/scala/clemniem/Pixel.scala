package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** RGBA pixel for PixelPic; alpha 0–255. */
final case class Pixel(r: Int, g: Int, b: Int, a: Int = 255) {
  def brightness: Double =
    (0.299 * r + 0.587 * g + 0.114 * b) * (a / 255.0)
}

object Pixel {
  given Encoder[Pixel] = deriveEncoder
  given Decoder[Pixel] = deriveDecoder
}
