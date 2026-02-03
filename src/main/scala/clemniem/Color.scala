package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Wrapper around RGB values with hex conversion. Components clamped to 0–255. */
final case class Color(r: Int, g: Int, b: Int) {

  /** Hex string in the form #rrggbb (lowercase). */
  def toHex: String = {
    def pad(n: Int): String = {
      val h = (n & 0xff).toHexString
      if (h.length >= 2) h else "0" + h
    }
    s"#${pad(r)}${pad(g)}${pad(b)}"
  }
}

object Color {

  private def clamp(c: Int): Int = math.max(0, math.min(255, c))

  /** Parse hex string: #rrggbb or #rgb or rrggbb (with or without #). Returns black on parse failure. */
  def fromHex(hex: String): Color =
    try {
      val s = hex.trim
      val stripped = if (s.startsWith("#")) s.drop(1) else s
      if (stripped.length == 6) {
        val r = Integer.parseInt(stripped.take(2), 16)
        val g = Integer.parseInt(stripped.slice(2, 4), 16)
        val b = Integer.parseInt(stripped.drop(4), 16)
        Color(clamp(r), clamp(g), clamp(b))
      } else if (stripped.length == 3) {
        val r = Integer.parseInt(stripped(0).toString * 2, 16)
        val g = Integer.parseInt(stripped(1).toString * 2, 16)
        val b = Integer.parseInt(stripped(2).toString * 2, 16)
        Color(clamp(r), clamp(g), clamp(b))
      } else
        Color(0, 0, 0)
    } catch { case _: NumberFormatException => Color(0, 0, 0) }

  /** Try to parse hex; returns None if invalid or blank. */
  def fromHexOption(hex: String): Option[Color] =
    Option(hex).filter(s => !s.isBlank).map(fromHex)

  given Encoder[Color] = deriveEncoder
  given Decoder[Color] = deriveDecoder
}
