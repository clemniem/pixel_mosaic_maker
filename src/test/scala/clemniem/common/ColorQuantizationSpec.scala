package clemniem.common

import clemniem.common.image.{ColorQuantizationService, NoColorDithering, RawImage}
import munit.FunSuite

/** Tests for ColorQuantizationService.medianCutPalette short-circuit and the general path.
  *
  * Key invariant: when the source has ≤ numColors unique colours, the short-circuit returns those exact colours without
  * median-cut or k-means, preventing skewed-histogram degeneracy (bug: duplicate centroids → 4→2 collapse).
  */
class ColorQuantizationSpec extends FunSuite {

  /** Build a RawImage from a colour map: each colour covers exactly `count` pixels. Uses width=totalPixels, height=1 to
    * avoid any padding.
    */
  private def buildRaw(colourCounts: List[((Int, Int, Int), Int)]): RawImage = {
    val data = colourCounts
      .flatMap { case ((r, g, b), count) =>
        List.fill(count)(List(r.toByte, g.toByte, b.toByte, 255.toByte))
      }
      .flatten
      .toArray
    val total = data.length / 4
    RawImage(total, 1, data)
  }

  /** RGB triples extracted from a QuantizedResult palette (alpha ignored). */
  private def paletteRgbSet(palette: Vector[(Byte, Byte, Byte, Byte)]): Set[(Int, Int, Int)] =
    palette.map { case (r, g, b, _) => (r & 0xff, g & 0xff, b & 0xff) }.toSet

  // The four exact colours from the moon image.
  private val moonColours: List[((Int, Int, Int), Int)] = List(
    ((47, 28, 53), 22013),
    ((161, 168, 184), 483),
    ((81, 79, 108), 316),
    ((255, 191, 152), 228)
  )

  test("medianCutPalette short-circuit: skewed 4-colour image at N=4 returns the exact 4 source colours") {
    // Without the short-circuit, median-cut on the moon distribution produces 3 duplicate purple centroids.
    val raw    = buildRaw(moonColours)
    val result = ColorQuantizationService.medianCutPalette(raw, 4)
    assertEquals(result.size, 4)
    val outSet = paletteRgbSet(result)
    val srcSet = moonColours.map(_._1).toSet
    assertEquals(outSet, srcSet)
  }

  test("medianCutPalette short-circuit: skewed 4-colour image at N=16 still returns only 4 (not 16 duplicates)") {
    val raw    = buildRaw(moonColours)
    val result = ColorQuantizationService.medianCutPalette(raw, 16)
    assertEquals(result.size, 4, s"expected 4 distinct colours, got ${result.size}: $result")
    val outSet = paletteRgbSet(result)
    val srcSet = moonColours.map(_._1).toSet
    assertEquals(outSet, srcSet)
  }

  test("medianCutPalette short-circuit: quantize wrapper at N=4 produces 4 distinct palette entries") {
    val raw    = buildRaw(moonColours)
    val result = ColorQuantizationService.quantize(raw, 4, NoColorDithering)
    val outSet = paletteRgbSet(result.palette)
    val srcSet = moonColours.map(_._1).toSet
    assertEquals(outSet, srcSet)
    // Every pixel index must reference a valid palette slot
    assert(result.indices.forall(i => i >= 0 && i < result.palette.size))
    // All 4 palette slots are referenced
    assertEquals(result.indices.toSet.size, 4)
  }

  test("medianCutPalette general path: 1000+ unique colours still falls through to median-cut (no short-circuit)") {
    // Build a gradient: each pixel has a unique or near-unique RGB so distinct.size >> numColors.
    val side = 32
    val data = new Array[Byte](side * side * 4)
    for (i <- 0 until side * side) {
      data(i * 4) = (i             % 256).toByte
      data(i * 4 + 1) = ((i / 256) % 256).toByte
      data(i * 4 + 2) = ((i * 7)   % 256).toByte
      data(i * 4 + 3) = 255.toByte
    }
    val raw    = RawImage(side, side, data)
    val result = ColorQuantizationService.medianCutPalette(raw, 4)
    // Short-circuit should NOT fire (source has 1024 unique colours, 4 is far less than that).
    // Result must be 4 distinct averaged centroids, none of which need match a source pixel exactly.
    assertEquals(result.size, 4)
    assertEquals(result.distinct.size, 4, s"median-cut produced duplicate centroids on gradient: $result")
    // Sanity: centroids are in the 0-255 byte range (trivially true for Byte, but explicit)
    result.foreach { case (r, g, b, a) =>
      assert((r & 0xff) <= 255 && (g & 0xff) <= 255 && (b & 0xff) <= 255 && (a & 0xff) == 255)
    }
  }

  test("medianCutPalette: 2-colour image at N=4 returns 2 entries (short-circuit, no padding to N)") {
    val twoColours: List[((Int, Int, Int), Int)] = List(((0, 0, 0), 500), ((255, 255, 255), 500))
    val raw                                      = buildRaw(twoColours)
    val result                                   = ColorQuantizationService.medianCutPalette(raw, 4)
    assertEquals(result.size, 2)
    assertEquals(paletteRgbSet(result), Set((0, 0, 0), (255, 255, 255)))
  }
}
