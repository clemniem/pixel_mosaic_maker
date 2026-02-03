package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class ImgSize(width: Int, height: Int) {
  def ratio: Double          = if (height == 0) 0.0 else width.toDouble / height
  def scale(s: Int): ImgSize = ImgSize(width * s, height * s)
  /** Whether this size is an integer multiple of the given size (e.g. nearest-neighbor upscale). */
  def matchesAspectRatio(other: ImgSize): Option[Int] =
    if (width > 0 && height > 0 && other.width > 0 && other.height > 0 &&
        width % other.width == 0 && height % other.height == 0)
      Some(width / other.width)
    else
      None
}

object ImgSize {
  given Encoder[ImgSize] = deriveEncoder
  given Decoder[ImgSize] = deriveDecoder
}
