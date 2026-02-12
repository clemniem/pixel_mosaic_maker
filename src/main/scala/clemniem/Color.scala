package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Wrapper around RGB values with hex conversion. Components clamped to 0–255. */
final case class Color(r: Int, g: Int, b: Int) {

  /** RGB as a tuple for APIs that expect (r, g, b). */
  def rgb: (Int, Int, Int) = (r, g, b)

  /** Hex string in the form #rrggbb (lowercase). */
  def toHex: String = {
    def pad(n: Int): String = {
      val h = (n & 0xff).toHexString
      if (h.length >= 2) h else "0" + h
    }
    s"#${pad(r)}${pad(g)}${pad(b)}"
  }

  /** CSS/canvas rgba string: "rgba(r,g,b,a)". Alpha clamped to 0.0–1.0. */
  def rgba(alpha: Double): String = {
    val a = math.max(0.0, math.min(1.0, alpha))
    s"rgba($r,$g,$b,$a)"
  }
}

object Color {

  /** Central definitions for fixed colors used across the app (PDF, UI, etc.). */
  val black: Color                           = Color(0, 0, 0)
  val white: Color                           = Color(255, 255, 255)
  val smallOverviewGrey: Color               = Color(210, 210, 210)
  val progressBarBackgroundPastelBlue: Color = Color(200, 220, 240)
  val progressBarFill: Color                 = Color(0, 0, 0)
  val layerPatchBackground: Color           = Color(220, 220, 220)
  val defaultPageBackground: Color          = Color(253, 251, 230)
  /** Red used for error/validation strokes (e.g. canvas overlay). */
  val errorStroke: Color                  = Color(255, 0, 0)
  /** Green used for highlight/success strokes (e.g. current step overlay). */
  val highlightStroke: Color              = Color(0, 200, 0)

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

  /** Normalize a hex string to #rrggbb for use in `<input type="color">`. Returns `default` if invalid. */
  def normalizeHex(hex: String, default: String): String = {
    val s        = hex.trim
    val withHash = if (s.startsWith("#")) s else "#" + s
    if (withHash.length == 7) withHash else default
  }

  given Encoder[Color] = deriveEncoder
  given Decoder[Color] = deriveDecoder
}
