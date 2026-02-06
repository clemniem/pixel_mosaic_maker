package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class Rectangle(x: Int, y: Int, width: Int, height: Int)

object Rectangle {
  given Encoder[Rectangle] = deriveEncoder
  given Decoder[Rectangle] = deriveDecoder
}
