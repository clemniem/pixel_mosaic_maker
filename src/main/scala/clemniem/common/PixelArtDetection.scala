package clemniem.common

import org.scalajs.dom.ImageData

import scala.util.boundary
import scala.util.boundary.break

/** Pixel-art nearest-neighbor scale detection. Extracted from [[ImageUtils]] so the framework module has no domain-type
  * imports.
  */
object PixelArtDetection {

  /** Detect if image is nearest-neighbor scaled by an integer factor (2-10). */
  def detectNearestNeighborScale(imageData: ImageData): Option[Int] = {
    val w = imageData.width
    val h = imageData.height
    val d = imageData.data
    detectNearestNeighborScaleFromBytesWithTolerance(w, h, i => (d(i) & 0xff).toByte, 0)
  }

  /** Same as [[detectNearestNeighborScale]] but on raw bytes (no DOM). Used for unit tests. Strict equality. */
  def detectNearestNeighborScaleFromBytes(width: Int, height: Int, data: Array[Byte]): Option[Int] =
    detectNearestNeighborScaleFromBytesWithTolerance(width, height, i => (data(i) & 0xff).toByte, 0)

  /** Same as [[detectNearestNeighborScaleFromBytes]] but allows per-channel deviation up to `tolerance`. Useful when
    * source bytes come from a canvas (sRGB pipeline may shift values by a few units).
    */
  def detectNearestNeighborScaleFromBytes(width: Int, height: Int, data: Array[Byte], tolerance: Int): Option[Int] =
    detectNearestNeighborScaleFromBytesWithTolerance(width, height, i => (data(i) & 0xff).toByte, tolerance)

  /** Iterate from the LARGEST candidate factor (10) down to 2 and return the first match.
    *
    * A nearest-neighbor F-times upscale by definition also satisfies the test for every divisor of F (e.g. an image
    * upscaled by 4 also has uniform 2x2 blocks). Picking the smallest match would return 2 for a 4x upscale and only
    * recover half the resolution; picking the largest match returns the true logical factor.
    */
  private def detectNearestNeighborScaleFromBytesWithTolerance(
    width: Int,
    height: Int,
    getByte: Int => Byte,
    tolerance: Int
  ): Option[Int] =
    boundary {
      for (scaleFactor <- 10 to 2 by -1) {
        val pw = width / scaleFactor
        val ph = height / scaleFactor
        if (pw >= 1 && ph >= 1) {
          val allMatch = (0 until ph).forall { y =>
            (0 until pw).forall { x =>
              val index = ((y * scaleFactor) * width + (x * scaleFactor)) * 4
              val refR  = getByte(index) & 0xff
              val refG  = getByte(index + 1) & 0xff
              val refB  = getByte(index + 2) & 0xff
              val refA  = getByte(index + 3) & 0xff
              (0 until scaleFactor).forall { dy =>
                (0 until scaleFactor).forall { dx =>
                  val ti = ((y * scaleFactor + dy) * width + (x * scaleFactor + dx)) * 4
                  math.abs((getByte(ti) & 0xff) - refR) <= tolerance &&
                  math.abs((getByte(ti + 1) & 0xff) - refG) <= tolerance &&
                  math.abs((getByte(ti + 2) & 0xff) - refB) <= tolerance &&
                  math.abs((getByte(ti + 3) & 0xff) - refA) <= tolerance
                }
              }
            }
          }
          if (allMatch) break(Some(scaleFactor))
        }
      }
      None
    }
}
