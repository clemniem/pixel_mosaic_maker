package clemniem.common

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
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(4, 4, data), Some(2))
  }

  test("detectNearestNeighborScaleFromBytes detects factor 2 on 4×4 that is 2×2 repeated") {
    val data = scaled2x2()
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(4, 4, data), Some(2))
  }

  test("detectNearestNeighborScaleFromBytes returns None for non-scaled 2×2") {
    val data = rgba(2, 2)((x, y) => (if (x == y) 255.toByte else 0.toByte, 0.toByte, 128.toByte, 255.toByte))
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(2, 2, data), None)
  }

  test("detectNearestNeighborScaleFromBytes returns None when scale would be < 1 (small image)") {
    val data = new Array[Byte](1 * 1 * 4)
    data(0) = 255.toByte
    data(1) = 0.toByte
    data(2) = 0.toByte
    data(3) = 255.toByte
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(1, 1, data), None)
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
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(nw, nh, small), None)
  }

  test("round-trip: 2×2 upscaled to 4×4 then detected and downscaled matches logical 2×2") {
    val data4x4 = scaled2x2()
    assertEquals(ImageUtils.detectNearestNeighborScaleFromBytes(4, 4, data4x4), Some(2))
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
