package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class ImgSize(width: Int, height: Int)

object ImgSize {
  given Encoder[ImgSize] = deriveEncoder
  given Decoder[ImgSize] = deriveDecoder
}
