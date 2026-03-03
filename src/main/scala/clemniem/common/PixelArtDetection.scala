package clemniem.common

import org.scalajs.dom.ImageData

import scala.util.boundary
import scala.util.boundary.break

/** Pixel-art nearest-neighbor scale detection. Extracted from [[ImageUtils]] so the framework module has no
  * domain-type imports.
  */
object PixelArtDetection {

  /** Detect if image is nearest-neighbor scaled by an integer factor (2-10). */
  def detectNearestNeighborScale(imageData: ImageData): Option[Int] = {
    val w = imageData.width
    val h = imageData.height
    val d = imageData.data
    detectNearestNeighborScaleFromBytes(w, h, i => (d(i) & 0xff).toByte)
  }

  /** Same as [[detectNearestNeighborScale]] but on raw bytes (no DOM). Used for unit tests. */
  def detectNearestNeighborScaleFromBytes(width: Int, height: Int, data: Array[Byte]): Option[Int] =
    detectNearestNeighborScaleFromBytes(width, height, i => (data(i) & 0xff).toByte)

  private def detectNearestNeighborScaleFromBytes(width: Int, height: Int, getByte: Int => Byte): Option[Int] =
    boundary {
      for (scaleFactor <- 2 to 10) {
        val pw = width / scaleFactor
        val ph = height / scaleFactor
        if (pw >= 1 && ph >= 1) {
          val allMatch = (0 until ph).forall { y =>
            (0 until pw).forall { x =>
              val index = ((y * scaleFactor) * width + (x * scaleFactor)) * 4
              val ref   = (getByte(index), getByte(index + 1), getByte(index + 2), getByte(index + 3))
              (0 until scaleFactor).forall { dy =>
                (0 until scaleFactor).forall { dx =>
                  val testIndex = ((y * scaleFactor + dy) * width + (x * scaleFactor + dx)) * 4
                  val test =
                    (getByte(testIndex), getByte(testIndex + 1), getByte(testIndex + 2), getByte(testIndex + 3))
                  test == ref
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
