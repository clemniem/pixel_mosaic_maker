package clemniem.common

import clemniem.common.image.{DownscalePixelPerfect, RawImage, SizeReductionService}
import munit.FunSuite

/** Unit tests for nearest-neighbor scale detection and downscaling (ImageUtils). Uses synthetic byte arrays so tests
  * run without DOM.
  *
  * Example assets for manual verification: leaf_x1 (base resolution) and leaf_x2 (2× scaled) e.g. assets/leaf_x1-*.png
  * and assets/leaf_x2-*.png. Loading leaf_x2, detecting factor 2, and downscaling should yield the same pixel
  * dimensions and content as leaf_x1.
  */
class ResizeSpec extends FunSuite {

  /** Build RGBA byte array (row-major, 4 bytes per pixel). */
  private def rgba(w: Int, h: Int)(fill: (Int, Int) => (Byte, Byte, Byte, Byte)): Array[Byte] = {
    val out = new Array[Byte](w * h * 4)
    for {
      y <- 0 until h
      x <- 0 until w
    } {
      val (r, g, b, a) = fill(x, y)
      val i            = (y * w + x) * 4
      out(i) = r
      out(i + 1) = g
      out(i + 2) = b
      out(i + 3) = a
    }
    out
  }

  /** 2×2 image scaled up to 4×4 (each logical pixel is a 2×2 block). */
  private def scaled2x2(): Array[Byte] = {
    // Logical 2×2: (0,0)=red, (1,0)=green, (0,1)=blue, (1,1)=white
    val r       = (255.toByte, 0.toByte, 0.toByte, 255.toByte)
    val g       = (0.toByte, 255.toByte, 0.toByte, 255.toByte)
    val b       = (0.toByte, 0.toByte, 255.toByte, 255.toByte)
    val w       = (255.toByte, 255.toByte, 255.toByte, 255.toByte)
    val logical = Array(Array(r, g), Array(b, w))
    val out     = new Array[Byte](4 * 4 * 4)
    for {
      y <- 0 until 4
      x <- 0 until 4
    } {
      val (lr, lg, lb, la) = logical(y / 2)(x / 2)
      val i                = (y * 4 + x) * 4
      out(i) = lr
      out(i + 1) = lg
      out(i + 2) = lb
      out(i + 3) = la
    }
    out
  }

  test("detectNearestNeighborScaleFromBytes detects factor 2 on 4×4 uniform (all same pixel)") {
    val data = rgba(4, 4)((_, _) => (100.toByte, 101.toByte, 102.toByte, 255.toByte))
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(4, 4, data), Some(2))
  }

  test("detectNearestNeighborScaleFromBytes detects factor 2 on 4×4 that is 2×2 repeated") {
    val data = scaled2x2()
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(4, 4, data), Some(2))
  }

  test("detectNearestNeighborScaleFromBytes returns None for non-scaled 2×2") {
    val data = rgba(2, 2)((x, y) => (if (x == y) 255.toByte else 0.toByte, 0.toByte, 128.toByte, 255.toByte))
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(2, 2, data), None)
  }

  test("detectNearestNeighborScaleFromBytes returns None when scale would be < 1 (small image)") {
    val data = new Array[Byte](1 * 1 * 4)
    data(0) = 255.toByte
    data(1) = 0.toByte
    data(2) = 0.toByte
    data(3) = 255.toByte
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(1, 1, data), None)
  }

  test("downscaleToBytes reduces 4×4 to 2×2 by factor 2") {
    val data          = scaled2x2()
    val (nw, nh, out) = ImageUtils.downscaleToBytes(4, 4, data, 2)
    assertEquals(nw, 2)
    assertEquals(nh, 2)
    assertEquals(out.length, 2 * 2 * 4)
    // Top-left pixel (0,0) should be red
    assertEquals(out(0), 255.toByte)
    assertEquals(out(1), 0.toByte)
    assertEquals(out(2), 0.toByte)
    assertEquals(out(3), 255.toByte)
    // (1,0) green
    assertEquals(out(4), 0.toByte)
    assertEquals(out(5), 255.toByte)
    assertEquals(out(6), 0.toByte)
    assertEquals(out(7), 255.toByte)
    // (0,1) blue
    assertEquals(out(8), 0.toByte)
    assertEquals(out(9), 0.toByte)
    assertEquals(out(10), 255.toByte)
    assertEquals(out(11), 255.toByte)
    // (1,1) white
    assertEquals(out(12), 255.toByte)
    assertEquals(out(13), 255.toByte)
    assertEquals(out(14), 255.toByte)
    assertEquals(out(15), 255.toByte)
  }

  test("downscale then detect: 4×4 scaled-by-2 downscaled to 2×2 is not detected as scaled") {
    val data            = scaled2x2()
    val (nw, nh, small) = ImageUtils.downscaleToBytes(4, 4, data, 2)
    assertEquals(nw, 2)
    assertEquals(nh, 2)
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(nw, nh, small), None)
  }

  /** Unique RGBA colors in a RawImage, encoded as Long for hashing. */
  private def uniqueColors(img: RawImage): Set[Long] = {
    val set = scala.collection.mutable.HashSet.empty[Long]
    for (i <- 0 until img.pixelCount) {
      val o = i * 4
      val r = img.data(o) & 0xff
      val g = img.data(o + 1) & 0xff
      val b = img.data(o + 2) & 0xff
      val a = img.data(o + 3) & 0xff
      set += ((r.toLong << 24) | (g.toLong << 16) | (b.toLong << 8) | a.toLong)
    }
    set.toSet
  }

  test("DownscalePixelPerfect: 4×4 scaled-by-2 round-trips to original logical 2×2") {
    val raw = RawImage(4, 4, scaled2x2())
    val out = SizeReductionService.downscale(raw, 500, 500, DownscalePixelPerfect)
    assertEquals(out.width, 2)
    assertEquals(out.height, 2)
    val expected = Array(
      255.toByte, 0.toByte, 0.toByte, 255.toByte,
      0.toByte, 255.toByte, 0.toByte, 255.toByte,
      0.toByte, 0.toByte, 255.toByte, 255.toByte,
      255.toByte, 255.toByte, 255.toByte, 255.toByte
    )
    assert(out.data.sameElements(expected), s"expected ${expected.toSeq}, got ${out.data.toSeq}")
  }

  test("DownscalePixelPerfect: 4-color 8×8 (4×4 blocks) preserves exactly 4 colors") {
    val red    = (255.toByte, 0.toByte, 0.toByte, 255.toByte)
    val green  = (0.toByte, 255.toByte, 0.toByte, 255.toByte)
    val blue   = (0.toByte, 0.toByte, 255.toByte, 255.toByte)
    val yellow = (255.toByte, 255.toByte, 0.toByte, 255.toByte)
    val logical = Array(Array(red, green), Array(blue, yellow))
    val data = rgba(8, 8) { (x, y) =>
      logical(y / 4)(x / 4)
    }
    val raw = RawImage(8, 8, data)
    val out = SizeReductionService.downscale(raw, 500, 500, DownscalePixelPerfect)
    assertEquals(uniqueColors(out).size, 4)
  }

  test("DownscalePixelPerfect: non-aligned 5×5 source only contains colors from the source") {
    val red   = (255.toByte, 0.toByte, 0.toByte, 255.toByte)
    val green = (0.toByte, 255.toByte, 0.toByte, 255.toByte)
    val blue  = (0.toByte, 0.toByte, 255.toByte, 255.toByte)
    val palette = Array(red, green, blue)
    val data = rgba(5, 5) { (x, y) =>
      palette((x + y) % 3)
    }
    val raw     = RawImage(5, 5, data)
    val src     = uniqueColors(raw)
    val out     = SizeReductionService.downscale(raw, 3, 3, DownscalePixelPerfect)
    assert(out.width <= 3 && out.height <= 3, s"expected ≤ 3×3, got ${out.width}×${out.height}")
    val outset = uniqueColors(out)
    assert(outset.subsetOf(src), s"output colors $outset not subset of source $src")
  }

  /** Add per-channel noise clamped to [0, 255], keeping alpha at 255. */
  private def addNoise(data: Array[Byte], noise: Int): Array[Byte] = {
    val out    = data.clone()
    val rng    = new scala.util.Random(42)
    val nPx    = data.length / 4
    for (i <- 0 until nPx) {
      val o = i * 4
      out(o) = (((data(o) & 0xff) + rng.nextInt(2 * noise + 1) - noise).max(0).min(255)).toByte
      out(o + 1) = (((data(o + 1) & 0xff) + rng.nextInt(2 * noise + 1) - noise).max(0).min(255)).toByte
      out(o + 2) = (((data(o + 2) & 0xff) + rng.nextInt(2 * noise + 1) - noise).max(0).min(255)).toByte
      out(o + 3) = data(o + 3)
    }
    out
  }

  test("tolerant detect: noisy 4×4 (±2 noise) detected with tolerance 8, not with 0") {
    val clean = scaled2x2()
    val noisy = addNoise(clean, 2)
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(4, 4, noisy, 8), Some(2))
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(4, 4, noisy, 0), None)
  }

  test("DownscalePixelPerfect: 8×8 (factor-2 NN upscale, uniform sRGB-style colour shift) output has exactly 4 colours") {
    // Realistic sRGB simulation: all pixels of the same original colour get the SAME fixed shift,
    // so each 2×2 block remains identical and the factor-2 upscale is still detected.
    val red   = (202.toByte, 1.toByte, 1.toByte, 255.toByte)
    val green = (1.toByte, 199.toByte, 1.toByte, 255.toByte)
    val blue  = (1.toByte, 1.toByte, 197.toByte, 255.toByte)
    val white = (201.toByte, 200.toByte, 199.toByte, 255.toByte)
    def logicalColor(lx: Int, ly: Int): (Byte, Byte, Byte, Byte) =
      if (lx < 2 && ly < 2) red
      else if (lx >= 2 && ly < 2) green
      else if (lx < 2 && ly >= 2) blue
      else white
    val data = rgba(8, 8) { (x, y) => logicalColor(x / 2, y / 2) }
    val raw  = RawImage(8, 8, data)
    val out  = SizeReductionService.downscale(raw, 500, 500, DownscalePixelPerfect)
    assertEquals(uniqueColors(out).size, 4)
  }

  test("DownscalePixelPerfect: mode-filter output colours are always a subset of source colours") {
    // 9×9 image with a 3×3 grid of 3×3 solid-colour blocks (no straddling when downscaled to 3×3).
    val red   = (180.toByte, 0.toByte, 0.toByte, 255.toByte)
    val green = (0.toByte, 180.toByte, 0.toByte, 255.toByte)
    val blue  = (0.toByte, 0.toByte, 180.toByte, 255.toByte)
    val palette = Array(red, green, blue)
    val data   = rgba(9, 9) { (x, y) => palette(((x / 3) + (y / 3)) % 3) }
    val raw    = RawImage(9, 9, data)
    val srcSet = uniqueColors(raw)
    val out    = SizeReductionService.downscale(raw, 3, 3, DownscalePixelPerfect)
    assertEquals(out.width, 3)
    assertEquals(out.height, 3)
    val outSet = uniqueColors(out)
    assert(outSet.subsetOf(srcSet), s"output colours $outSet not subset of source $srcSet")
  }

  test("round-trip: 2×2 upscaled to 4×4 then detected and downscaled matches logical 2×2") {
    val data4x4 = scaled2x2()
    assertEquals(PixelArtDetection.detectNearestNeighborScaleFromBytes(4, 4, data4x4), Some(2))
    val (nw, nh, out) = ImageUtils.downscaleToBytes(4, 4, data4x4, 2)
    assertEquals(nw, 2)
    assertEquals(nh, 2)
    val expected = Array(
      255.toByte,
      0.toByte,
      0.toByte,
      255.toByte,
      0.toByte,
      255.toByte,
      0.toByte,
      255.toByte,
      0.toByte,
      0.toByte,
      255.toByte,
      255.toByte,
      255.toByte,
      255.toByte,
      255.toByte,
      255.toByte
    )
    assert(out.sameElements(expected), s"expected ${expected.toSeq}, got ${out.toSeq}")
  }
}
