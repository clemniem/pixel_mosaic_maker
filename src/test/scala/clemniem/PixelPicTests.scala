package clemniem

import munit.FunSuite

class PixelPicTests extends FunSuite {

  private val black = Pixel(0, 0, 0, 255)
  private val white = Pixel(255, 255, 255, 255)
  private val red   = Pixel(255, 0, 0, 255)
  private val palette2 = Vector(black, white)
  private val palette3 = Vector(black, white, red)

  /** 2×2 image: top-left black, rest white. Pixel indices row-major [0,1,1,0]. */
  private def pic2x2: Option[PixelPic] =
    PixelPic(
      width = 2,
      height = 2,
      paletteLookup = palette2,
      pixels = Vector(0, 1, 1, 0),
      pixelCounts = Map(0 -> 2, 1 -> 2),
      name = "test-2x2"
    )

  /** 3×2 image, 3 colors, 6 pixels. */
  private def pic3x2: Option[PixelPic] =
    PixelPic(
      width = 3,
      height = 2,
      paletteLookup = palette3,
      pixels = Vector(0, 1, 2, 1, 1, 0),
      pixelCounts = Map(0 -> 2, 1 -> 3, 2 -> 1),
      name = "test-3x2"
    )

  test("PixelPic.apply accepts valid 2×2 image") {
    val result = pic2x2
    assert(result.isDefined)
    val p = result.get
    assertEquals(p.width, 2)
    assertEquals(p.height, 2)
    assertEquals(p.pixels.length, 4)
    assertEquals(p.paletteLookup.size, 2)
  }

  test("PixelPic.apply rejects when width * height != pixels.length") {
    val result = PixelPic(
      width = 2,
      height = 2,
      paletteLookup = palette2,
      pixels = Vector(0, 1, 1),
      pixelCounts = Map(0 -> 2, 1 -> 1),
      name = "bad"
    )
    assert(result.isEmpty)
  }

  test("PixelPic.apply rejects empty palette") {
    val result = PixelPic(
      width = 2,
      height = 2,
      paletteLookup = Vector.empty,
      pixels = Vector(0, 1, 1, 0),
      pixelCounts = Map(0 -> 2, 1 -> 2),
      name = "bad"
    )
    assert(result.isEmpty)
  }

  test("PixelPic.apply rejects pixel index out of palette range") {
    val result = PixelPic(
      width = 2,
      height = 2,
      paletteLookup = palette2,
      pixels = Vector(0, 1, 2, 0),
      pixelCounts = Map(0 -> 2, 1 -> 1, 2 -> 1),
      name = "bad"
    )
    assert(result.isEmpty)
  }

  test("PixelPic.apply rejects negative pixel index") {
    val result = PixelPic(
      width = 2,
      height = 2,
      paletteLookup = palette2,
      pixels = Vector(0, -1, 1, 0),
      pixelCounts = Map(0 -> 2, 1 -> 1),
      name = "bad"
    )
    assert(result.isEmpty)
  }

  test("size returns ImgSize matching dimensions") {
    val p = pic2x2.get
    assertEquals(p.size, ImgSize(2, 2))
    val q = pic3x2.get
    assertEquals(q.size, ImgSize(3, 2))
  }

  test("getPixel returns correct color at (x, y)") {
    val p = pic2x2.get
    assertEquals(p.getPixel(0, 0), black)
    assertEquals(p.getPixel(1, 0), white)
    assertEquals(p.getPixel(0, 1), white)
    assertEquals(p.getPixel(1, 1), black)
  }

  test("getIndexByCount returns palette indices sorted by count ascending") {
    val p = pic3x2.get
    val byCount = p.getIndexByCount
    assertEquals(byCount, Vector(2, 0, 1))
  }

  test("crop in bounds returns Some with correct dimensions and pixels") {
    val p = pic3x2.get
    val cropped = p.crop(1, 0, 2, 2)
    assert(cropped.isDefined)
    val c = cropped.get
    assertEquals(c.width, 2)
    assertEquals(c.height, 2)
    assertEquals(c.pixels.length, 4)
    assertEquals(c.getPixel(0, 0), white)
    assertEquals(c.getPixel(1, 0), red)
    assertEquals(c.name, "test-3x2_crop_1_0_2_2")
  }

  test("crop full image returns copy with same dimensions") {
    val p = pic2x2.get
    val cropped = p.crop(0, 0, 2, 2)
    assert(cropped.isDefined)
    val c = cropped.get
    assertEquals(c.width, p.width)
    assertEquals(c.height, p.height)
    assertEquals(c.pixels, p.pixels)
  }

  test("crop out of bounds (negative x) returns None") {
    val p = pic2x2.get
    assert(p.crop(-1, 0, 2, 2).isEmpty)
  }

  test("crop out of bounds (negative y) returns None") {
    val p = pic2x2.get
    assert(p.crop(0, -1, 2, 2).isEmpty)
  }

  test("crop out of bounds (x + w > width) returns None") {
    val p = pic2x2.get
    assert(p.crop(1, 0, 2, 2).isEmpty)
  }

  test("crop out of bounds (y + h > height) returns None") {
    val p = pic2x2.get
    assert(p.crop(0, 1, 2, 2).isEmpty)
  }

  test("crop(rect) delegates to crop(x, y, w, h)") {
    val p = pic3x2.get
    val rect = Rectangle(1, 0, 2, 2)
    assertEquals(p.crop(rect), p.crop(1, 0, 2, 2))
  }

  test("setPalette replaces palette up to original size") {
    val p = pic2x2.get
    val newPalette = Vector(red, black)
    val updated = p.setPalette(newPalette)
    assertEquals(updated.paletteLookup(0), red)
    assertEquals(updated.paletteLookup(1), black)
    assertEquals(updated.pixels, p.pixels)
  }

  test("setPalette with longer palette only takes first N colors") {
    val p = pic2x2.get
    val newPalette = Vector(red, black, white)
    val updated = p.setPalette(newPalette)
    assertEquals(updated.paletteLookup.size, 2)
    assertEquals(updated.paletteLookup(0), red)
    assertEquals(updated.paletteLookup(1), black)
  }

  test("pixelPalette maps Pixel to count") {
    val p = pic3x2.get
    val pp = p.pixelPalette
    assertEquals(pp(black), 2)
    assertEquals(pp(white), 3)
    assertEquals(pp(red), 1)
  }
}
