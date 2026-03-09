package clemniem.common

import clemniem.common.image.RawImage
import org.scalajs.dom.ImageData

/** Conversion helpers between [[ImageData]] and [[RawImage]]. Extracted from [[ImageUtils]] so the framework module has
  * no domain-type imports.
  */
object RawImageUtils {

  /** Convert ImageData to DOM-free RawImage (copy of bytes). */
  def rawFromImageData(imageData: ImageData): RawImage = {
    val w   = imageData.width
    val h   = imageData.height
    val src = imageData.data
    val arr = new Array[Byte](w * h * 4)
    for (i <- arr.indices) arr(i) = (src(i) & 0xff).toByte
    RawImage(w, h, arr)
  }

  /** Convert RawImage to ImageData (requires DOM for createImageData). */
  def imageDataFromRaw(raw: RawImage): ImageData = {
    val (_, ctx) = ImageUtils.createOffscreenCanvas(raw.width, raw.height)
    val out      = ctx.createImageData(raw.width, raw.height)
    val dst      = out.data
    for (i <- raw.data.indices) dst(i) = raw.data(i) & 0xff
    out
  }
}
