package clemniem.common.circe

import io.circe.{Decoder, Encoder}

import scala.annotation.unused

object MapAsList {
  given [K, V](using @unused encK: Encoder[K], @unused encV: Encoder[V]): Encoder[Map[K, V]] =
    Encoder.encodeList[(K, V)].contramap(_.toList)
  given [K, V](using @unused decK: Decoder[K], @unused decV: Decoder[V]): Decoder[Map[K, V]] =
    Decoder.decodeList[(K, V)].map(_.toMap)
}
