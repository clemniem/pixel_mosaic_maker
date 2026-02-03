package clemniem.common.circe

import io.circe.{Decoder, Encoder}

object MapAsList {
  given [K: Encoder: Decoder, V: Encoder: Decoder]: Encoder[Map[K, V]] =
    Encoder.encodeList[(K, V)].contramap(_.toList)
  given [K: Encoder: Decoder, V: Encoder: Decoder]: Decoder[Map[K, V]] =
    Decoder.decodeList[(K, V)].map(_.toMap)
}
