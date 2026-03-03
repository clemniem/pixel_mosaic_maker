package clemniem.common.circe

import io.circe.{Decoder, Encoder}

object MapAsList {
  given [K, V](using Encoder[K], Encoder[V]): Encoder[Map[K, V]] =
    Encoder.encodeList[(K, V)].contramap(_.toList)
  given [K, V](using Decoder[K], Decoder[V]): Decoder[Map[K, V]] =
    Decoder.decodeList[(K, V)].map(_.toMap)
}
