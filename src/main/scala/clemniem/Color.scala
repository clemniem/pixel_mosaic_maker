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

/** A named LEGO color with its RGB value. */
final case class LegoColor(name: String, color: Color) {
  /** Whether this is a transparent LEGO color. */
  def isTransparent: Boolean = name.startsWith("Trans-")
}

object LegoColor {

  /** Build a Color from a packed 0xRRGGBB integer. */
  private def hex(v: Int): Color = Color((v >> 16) & 0xff, (v >> 8) & 0xff, v & 0xff)

  val transDarkBlue: LegoColor = LegoColor("Trans-Dark Blue", hex(0x0020a0))
  val transOrange: LegoColor   = LegoColor("Trans-Orange", hex(0xf08f1c))
  val transRed: LegoColor      = LegoColor("Trans-Red", hex(0xc91a09))
  val transYellow: LegoColor   = LegoColor("Trans-Yellow", hex(0xf5cd2f))

  val allColors: List[LegoColor] = List(
    LegoColor("Black", hex(0x05131d)),
    LegoColor("Blue", hex(0x0055bf)),
    LegoColor("Green", hex(0x237841)),
    LegoColor("Dark Turquoise", hex(0x008f9b)),
    LegoColor("Red", hex(0xc91a09)),
    LegoColor("Dark Pink", hex(0xc870a0)),
    LegoColor("Brown", hex(0x583927)),
    LegoColor("Light Gray", hex(0x9ba19d)),
    LegoColor("Dark Gray", hex(0x6d6e5c)),
    LegoColor("Light Blue", hex(0xb4d2e3)),
    LegoColor("Bright Green", hex(0x4b9f4a)),
    LegoColor("Light Turquoise", hex(0x55a5af)),
    LegoColor("Salmon", hex(0xf2705e)),
    LegoColor("Pink", hex(0xfc97ac)),
    LegoColor("Yellow", hex(0xf2cd37)),
    LegoColor("White", hex(0xffffff)),
    LegoColor("Light Green", hex(0xc2dab8)),
    LegoColor("Light Yellow", hex(0xfbe696)),
    LegoColor("Tan", hex(0xe4cd9e)),
    LegoColor("Light Violet", hex(0xc9cae2)),
    LegoColor("Glow In Dark Opaque", hex(0xd4d5c9)),
    LegoColor("Purple", hex(0x81007b)),
    LegoColor("Dark Blue-Violet", hex(0x2032b0)),
    LegoColor("Orange", hex(0xfe8a18)),
    LegoColor("Magenta", hex(0x923978)),
    LegoColor("Lime", hex(0xbbe90b)),
    LegoColor("Dark Tan", hex(0x958a73)),
    LegoColor("Bright Pink", hex(0xe4adc8)),
    LegoColor("Medium Lavender", hex(0xac78ba)),
    LegoColor("Lavender", hex(0xe1d5ed)),
    LegoColor("Trans-Black IR Lens", hex(0x635f52)),
    LegoColor("Trans-Dark Blue", hex(0x0020a0)),
    LegoColor("Trans-Green", hex(0x84b68d)),
    LegoColor("Trans-Bright Green", hex(0xd9e4a7)),
    LegoColor("Trans-Brown", hex(0x635f52)),
    LegoColor("Trans-Light Blue", hex(0xaeefec)),
    LegoColor("Trans-Neon Green", hex(0xf8f184)),
    LegoColor("Trans-Very Lt Blue", hex(0xc1dff0)),
    LegoColor("Trans-Dark Pink", hex(0xdf6695)),
    LegoColor("Trans-Clear", hex(0xfcfcfc)),
    LegoColor("Trans-Purple", hex(0xa5a5cb)),
    LegoColor("Trans-Neon Yellow", hex(0xdab000)),
    LegoColor("Trans-Neon Orange", hex(0xff800d)),
    LegoColor("Chrome Antique Brass", hex(0x645a4c)),
    LegoColor("Chrome Blue", hex(0x6c96bf)),
    LegoColor("Chrome Green", hex(0x3cb371)),
    LegoColor("Chrome Pink", hex(0xaa4d8e)),
    LegoColor("Chrome Black", hex(0x1b2a34)),
    LegoColor("Very Light Orange", hex(0xf3cf9b)),
    LegoColor("Light Purple", hex(0xcd6298)),
    LegoColor("Reddish Brown", hex(0x582a12)),
    LegoColor("Light Bluish Gray", hex(0xa0a5a9)),
    LegoColor("Dark Bluish Gray", hex(0x6c6e68)),
    LegoColor("Medium Blue", hex(0x5a93db)),
    LegoColor("Medium Green", hex(0x73dca1)),
    LegoColor("Speckle Black-Copper", hex(0x05131d)),
    LegoColor("Speckle DBGray-Silver", hex(0x6c6e68)),
    LegoColor("Light Pink", hex(0xfecccf)),
    LegoColor("Light Nougat", hex(0xf6d7b3)),
    LegoColor("Milky White", hex(0xffffff)),
    LegoColor("Metallic Silver", hex(0xa5a9b4)),
    LegoColor("Metallic Green", hex(0x899b5f)),
    LegoColor("Metallic Gold", hex(0xdbac34)),
    LegoColor("Medium Nougat", hex(0xaa7d55)),
    LegoColor("Dark Purple", hex(0x3f3691)),
    LegoColor("Light Brown", hex(0x7c503a)),
    LegoColor("Royal Blue", hex(0x4c61db)),
    LegoColor("Nougat", hex(0xd09168)),
    LegoColor("Light Salmon", hex(0xfebabd)),
    LegoColor("Violet", hex(0x4354a3)),
    LegoColor("Medium Bluish Violet", hex(0x6874ca)),
    LegoColor("Glitter Trans-Dark Pink", hex(0xdf6695)),
    LegoColor("Medium Lime", hex(0xc7d23c)),
    LegoColor("Glitter Trans-Clear", hex(0xffffff)),
    LegoColor("Aqua", hex(0xb3d7d1)),
    LegoColor("Light Lime", hex(0xd9e4a7)),
    LegoColor("Light Orange", hex(0xf9ba61)),
    LegoColor("Glitter Trans-Purple", hex(0xa5a5cb)),
    LegoColor("Speckle Black-Silver", hex(0x05131d)),
    LegoColor("Speckle Black-Gold", hex(0x05131d)),
    LegoColor("Copper", hex(0xae7a59)),
    LegoColor("Pearl Light Gray", hex(0x9ca3a8)),
    LegoColor("Pearl Sand Blue", hex(0x7988a1)),
    LegoColor("Pearl Light Gold", hex(0xdcbc81)),
    LegoColor("Trans-Medium Blue", hex(0xcfe2f7)),
    LegoColor("Pearl Dark Gray", hex(0x575857)),
    LegoColor("Pearl Very Light Gray", hex(0xabadac)),
    LegoColor("Very Light Bluish Gray", hex(0xe6e3e0)),
    LegoColor("Yellowish Green", hex(0xdfeea5)),
    LegoColor("Flat Dark Gold", hex(0xb48455)),
    LegoColor("Flat Silver", hex(0x898788)),
    LegoColor("Pearl White", hex(0xf2f3f2)),
    LegoColor("Bright Light Orange", hex(0xf8bb3d)),
    LegoColor("Bright Light Blue", hex(0x9fc3e9)),
    LegoColor("Rust", hex(0xb31004)),
    LegoColor("Bright Light Yellow", hex(0xfff03a)),
    LegoColor("Trans-Pink", hex(0xe4adc8)),
    LegoColor("Sky Blue", hex(0x7dbfdd)),
    LegoColor("Trans-Light Purple", hex(0x96709f)),
    LegoColor("Dark Blue", hex(0x0a3463)),
    LegoColor("Dark Green", hex(0x184632)),
    LegoColor("Glow In Dark Trans", hex(0xbdc6ad)),
    LegoColor("Pearl Gold", hex(0xaa7f2e)),
    LegoColor("Dark Brown", hex(0x352100)),
    LegoColor("Maersk Blue", hex(0x3592c3)),
    LegoColor("Dark Red", hex(0x720e0f)),
    LegoColor("Dark Azure", hex(0x078bc9)),
    LegoColor("Medium Azure", hex(0x36aebf)),
    LegoColor("Light Aqua", hex(0xadc3c0)),
    LegoColor("Olive Green", hex(0x9b9a5a)),
    LegoColor("Chrome Gold", hex(0xbba53d)),
    LegoColor("Sand Red", hex(0xd67572)),
    LegoColor("Medium Dark Pink", hex(0xf785b1)),
    LegoColor("Earth Orange", hex(0xfa9c1c)),
    LegoColor("Sand Purple", hex(0x845e84)),
    LegoColor("Sand Green", hex(0xa0bcac)),
    LegoColor("Sand Blue", hex(0x6074a1)),
    LegoColor("Chrome Silver", hex(0xe0e0e0)),
    LegoColor("Fabuland Brown", hex(0xb67b50)),
    LegoColor("Medium Orange", hex(0xffa70b)),
    LegoColor("Dark Orange", hex(0xa95500)),
    LegoColor("Very Light Gray", hex(0xe6e3da)),
    LegoColor("Glow in Dark White", hex(0xd9d9d9)),
    LegoColor("Medium Violet", hex(0x9391e4)),
    LegoColor("Glitter Trans-Neon Green", hex(0xc0f500)),
    LegoColor("Glitter Trans-Light Blue", hex(0x68bcc5)),
    LegoColor("Trans-Flame Yellowish Orange", hex(0xfcb76d)),
    LegoColor("Trans-Fire Yellow", hex(0xfbe890)),
    LegoColor("Trans-Light Royal Blue", hex(0xb4d4f7)),
    LegoColor("Reddish Lilac", hex(0x8e5597)),
    LegoColor("Vintage Blue", hex(0x039cbd)),
    LegoColor("Vintage Green", hex(0x1e601e)),
    LegoColor("Vintage Red", hex(0xca1f08)),
    LegoColor("Vintage Yellow", hex(0xf3c305)),
    LegoColor("Fabuland Orange", hex(0xef9121)),
    LegoColor("Modulex White", hex(0xf4f4f4)),
    LegoColor("Modulex Light Bluish Gray", hex(0xafb5c7)),
    LegoColor("Modulex Light Gray", hex(0x9c9c9c)),
    LegoColor("Modulex Charcoal Gray", hex(0x595d60)),
    LegoColor("Modulex Tile Gray", hex(0x6b5a5a)),
    LegoColor("Modulex Black", hex(0x4d4c52)),
    LegoColor("Modulex Tile Brown", hex(0x330000)),
    LegoColor("Modulex Terracotta", hex(0x5c5030)),
    LegoColor("Modulex Brown", hex(0x907450)),
    LegoColor("Modulex Buff", hex(0xdec69c)),
    LegoColor("Modulex Red", hex(0xb52c20)),
    LegoColor("Modulex Pink Red", hex(0xf45c40)),
    LegoColor("Modulex Orange", hex(0xf47b30)),
    LegoColor("Modulex Light Orange", hex(0xf7ad63)),
    LegoColor("Modulex Light Yellow", hex(0xffe371)),
    LegoColor("Modulex Ochre Yellow", hex(0xfed557)),
    LegoColor("Modulex Lemon", hex(0xbdc618)),
    LegoColor("Modulex Pastel Green", hex(0x7db538)),
    LegoColor("Modulex Olive Green", hex(0x7c9051)),
    LegoColor("Modulex Aqua Green", hex(0x27867e)),
    LegoColor("Modulex Teal Blue", hex(0x467083)),
    LegoColor("Modulex Tile Blue", hex(0x0057a6)),
    LegoColor("Modulex Medium Blue", hex(0x61afff)),
    LegoColor("Modulex Pastel Blue", hex(0x68aece)),
    LegoColor("Modulex Violet", hex(0xbd7d85)),
    LegoColor("Modulex Pink", hex(0xf785b1)),
    LegoColor("Modulex Clear", hex(0xffffff)),
    LegoColor("Modulex Foil Dark Gray", hex(0x595d60)),
    LegoColor("Modulex Foil Light Gray", hex(0x9c9c9c)),
    LegoColor("Modulex Foil Dark Green", hex(0x006400)),
    LegoColor("Modulex Foil Light Green", hex(0x7db538)),
    LegoColor("Modulex Foil Dark Blue", hex(0x0057a6)),
    LegoColor("Modulex Foil Light Blue", hex(0x68aece)),
    LegoColor("Modulex Foil Violet", hex(0x4b0082)),
    LegoColor("Modulex Foil Red", hex(0x8b0000)),
    LegoColor("Modulex Foil Yellow", hex(0xfed557)),
    LegoColor("Modulex Foil Orange", hex(0xf7ad63)),
    LegoColor("Coral", hex(0xff698f)),
    LegoColor("Pastel Blue", hex(0x5ac4da)),
    LegoColor("Glitter Trans-Orange", hex(0xf08f1c)),
    LegoColor("Opal Trans-Light Blue", hex(0x68bcc5)),
    LegoColor("Opal Trans-Dark Pink", hex(0xce1d9b)),
    LegoColor("Opal Trans-Clear", hex(0xfcfcfc)),
    LegoColor("Opal Trans-Brown", hex(0x583927)),
    LegoColor("Trans-Light Bright Green", hex(0xc9e788)),
    LegoColor("Trans-Light Green", hex(0x94e5ab)),
    LegoColor("Opal Trans-Purple", hex(0x8320b7)),
    LegoColor("Opal Trans-Bright Green", hex(0x84b68d)),
    LegoColor("Opal Trans-Dark Blue", hex(0x0020a0)),
    LegoColor("Vibrant Yellow", hex(0xebd800)),
    LegoColor("Pearl Copper", hex(0xb46a00)),
    LegoColor("Fabuland Red", hex(0xff8014)),
    LegoColor("Reddish Gold", hex(0xac8247)),
    LegoColor("Curry", hex(0xdd982e)),
    LegoColor("Dark Nougat", hex(0xad6140)),
    LegoColor("Bright Reddish Orange", hex(0xee5434)),
    LegoColor("Pearl Red", hex(0xd60026)),
    LegoColor("Pearl Blue", hex(0x0059a3)),
    LegoColor("Pearl Green", hex(0x008e3c)),
    LegoColor("Pearl Brown", hex(0x57392c)),
    LegoColor("Pearl Black", hex(0x0a1327)),
    LegoColor("Duplo Blue", hex(0x009ece)),
    LegoColor("Duplo Medium Blue", hex(0x3e95b6)),
    LegoColor("Duplo Lime", hex(0xfff230)),
    LegoColor("Fabuland Lime", hex(0x78fc78)),
    LegoColor("Duplo Medium Green", hex(0x468a5f)),
    LegoColor("Duplo Light Green", hex(0x60ba76)),
    LegoColor("Light Tan", hex(0xf3c988)),
    LegoColor("Rust Orange", hex(0x872b17)),
    LegoColor("Clikits Pink", hex(0xfe78b0)),
    LegoColor("Two-tone Copper", hex(0x945148)),
    LegoColor("Two-tone Gold", hex(0xab673a)),
    LegoColor("Two-tone Silver", hex(0x737271)),
    LegoColor("Pearl Lime", hex(0x6a7944)),
    LegoColor("Duplo Pink", hex(0xff879c)),
    LegoColor("Medium Brown", hex(0x755945)),
    LegoColor("Warm Tan", hex(0xcca373)),
    LegoColor("Duplo Turquoise", hex(0x3fb69e)),
    LegoColor("Warm Yellowish Orange", hex(0xffcb78)),
    LegoColor("Metallic Copper", hex(0x764d3b)),
    LegoColor("Light Lilac", hex(0x9195ca)),
    LegoColor("Trans-Medium Purple", hex(0x8d73b3)),
    LegoColor("Trans-Black", hex(0x635f52)),
    LegoColor("Glitter Trans-Bright Green", hex(0xd9e4a7)),
    LegoColor("Glitter Trans-Medium Purple", hex(0x8d73b3)),
    LegoColor("Glitter Trans-Green", hex(0x84b68d)),
    LegoColor("Glitter Trans-Pink", hex(0xe4adc8)),
    LegoColor("Clikits Yellow", hex(0xffcf0b)),
    LegoColor("Duplo Dark Purple", hex(0x5f27aa)),
    LegoColor("Trans-Neon Red", hex(0xff0040)),
    LegoColor("Pearl Titanium", hex(0x3e3c39)),
    LegoColor("HO Aqua", hex(0xb3d7d1)),
    LegoColor("HO Azure", hex(0x1591cb)),
    LegoColor("HO Blue-gray", hex(0x354e5a)),
    LegoColor("HO Cyan", hex(0x5b98b3)),
    LegoColor("HO Dark Aqua", hex(0xa7dccf)),
    LegoColor("HO Dark Blue", hex(0x0a3463)),
    LegoColor("HO Dark Gray", hex(0x6d6e5c)),
    LegoColor("HO Dark Green", hex(0x184632)),
    LegoColor("HO Dark Lime", hex(0xb2b955)),
    LegoColor("HO Dark Red", hex(0x631314)),
    LegoColor("HO Dark Sand Green", hex(0x627a62)),
    LegoColor("HO Dark Turquoise", hex(0x10929d)),
    LegoColor("HO Earth Orange", hex(0xbb771b)),
    LegoColor("HO Gold", hex(0xb4a774)),
    LegoColor("HO Light Aqua", hex(0xa3d1c0)),
    LegoColor("HO Light Brown", hex(0x965336)),
    LegoColor("HO Light Gold", hex(0xcdc298)),
    LegoColor("HO Light Tan", hex(0xf9f1c7)),
    LegoColor("HO Light Yellow", hex(0xf5fab7)),
    LegoColor("HO Medium Blue", hex(0x7396c8)),
    LegoColor("HO Medium Red", hex(0xc01111)),
    LegoColor("HO Metallic Blue", hex(0x0d4763)),
    LegoColor("HO Metallic Dark Gray", hex(0x5e5e5e)),
    LegoColor("HO Metallic Green", hex(0x879867)),
    LegoColor("HO Metallic Sand Blue", hex(0x5f7d8c)),
    LegoColor("HO Olive Green", hex(0x9b9a5a)),
    LegoColor("HO Rose", hex(0xd06262)),
    LegoColor("HO Sand Blue", hex(0x6e8aa6)),
    LegoColor("HO Sand Green", hex(0xa0bcac)),
    LegoColor("HO Tan", hex(0xe4cd9e)),
    LegoColor("HO Titanium", hex(0x616161)),
    LegoColor("Metal", hex(0xa5adb4)),
    LegoColor("Reddish Orange", hex(0xca4c0b)),
    LegoColor("Sienna Brown", hex(0x915c3c)),
    LegoColor("Umber Brown", hex(0x5e3f33)),
    LegoColor("Opal Trans-Yellow", hex(0xf5cd2f)),
    LegoColor("[No Color/Any Color]", hex(0x05131d))
  ).sortBy(_.name)

  /** Find the nearest LEGO color to the given RGB color. */
  def nearest(c: Color): LegoColor =
    allColors.minBy { lc =>
      val dr = c.r - lc.color.r
      val dg = c.g - lc.color.g
      val db = c.b - lc.color.b
      dr * dr + dg * dg + db * db
    }
}