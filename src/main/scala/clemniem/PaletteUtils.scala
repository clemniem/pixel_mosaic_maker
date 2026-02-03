package clemniem

/** Helpers for applying stored palettes to pixel images. */
object PaletteUtils {

  /** Apply a stored palette to a PixelPic: map palette colors to Pixel (alpha 255), pad or trim to
    * the pic's palette size, then set the pic's palette.
    */
  def applyPaletteToPixelPic(pic: PixelPic, palette: StoredPalette): PixelPic = {
    val pixels = palette.colors.map(c => Pixel(c.r, c.g, c.b, 255)).toVector
    val needed = pic.paletteLookup.size
    val padded =
      if (pixels.size >= needed) pixels.take(needed)
      else pixels ++ Vector.fill(needed - pixels.size)(Pixel(0, 0, 0, 255))
    pic.setPalette(padded)
  }
}
