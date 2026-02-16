package clemniem.common

import cats.effect.IO
import clemniem.common.image.RawImage
import org.scalajs.dom
import org.scalajs.dom.{File, FileReader, ImageData}
import org.scalajs.dom.html.{Canvas, Image}

import scala.scalajs.js
import scala.util.boundary
import scala.util.boundary.break

object ImageUtils {

  type DataUrlBase64 = String
  type FileName      = String

  /** Create an offscreen canvas with the given dimensions and return (canvas, context). */
  def createOffscreenCanvas(w: Int, h: Int): (Canvas, dom.CanvasRenderingContext2D) = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = w
    canvas.height = h
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    (canvas, ctx)
  }

  def loadImageFromFile(file: File): IO[(FileName, DataUrlBase64)] =
    IO.async_[(FileName, DataUrlBase64)] { cb =>
      val reader = new FileReader()
      reader.onload = _ => cb(Right(file.name -> reader.result.toString))
      reader.readAsDataURL(file)
    }

  def getFromImage[T](base64: DataUrlBase64)(fun: Image => T): IO[T] =
    IO.async_[T] { cb =>
      val img = dom.document.createElement("img").asInstanceOf[Image]
      img.onload = _ => cb(Right(fun(img)))
      img.src = base64
    }

  /** Detect if image is nearest-neighbor scaled by an integer factor (2–10). */
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

  /** Downscale ImageData by taking every `factor`-th pixel (nearest-neighbor downscale). */
  def downscaleImageData(imageData: ImageData, factor: Int): ImageData = {
    val w                  = imageData.width
    val h                  = imageData.height
    val src                = imageData.data
    val arr                = Array.tabulate(w * h * 4)(i => (src(i) & 0xff).toByte)
    val (nw, nh, outBytes) = downscaleToBytes(w, h, arr, factor)
    val (_, ctx)           = createOffscreenCanvas(nw, nh)
    val out                = ctx.createImageData(nw, nh)
    val dst                = out.data
    for (i <- outBytes.indices) dst(i) = outBytes(i) & 0xff
    out
  }

  /** Downscale raw RGBA bytes by taking every `factor`-th pixel. Returns (newWidth, newHeight, newData). No DOM. */
  def downscaleToBytes(width: Int, height: Int, data: Array[Byte], factor: Int): (Int, Int, Array[Byte]) = {
    val nw     = width / factor
    val nh     = height / factor
    val outLen = nw * nh * 4
    val out    = new Array[Byte](outLen)
    for (i <- 0 until (nw * nh)) {
      val y      = i / nw
      val x      = i % nw
      val sy     = y * factor
      val sx     = x * factor
      val srcIdx = (sy * width + sx) * 4
      val dstIdx = i * 4
      out(dstIdx) = data(srcIdx)
      out(dstIdx + 1) = data(srcIdx + 1)
      out(dstIdx + 2) = data(srcIdx + 2)
      out(dstIdx + 3) = data(srcIdx + 3)
    }
    (nw, nh, out)
  }

  /** Get ImageData from a loaded Image. */
  def imageToImageData(img: Image): ImageData = {
    val (_, ctx) = createOffscreenCanvas(img.width, img.height)
    ctx.drawImage(img, 0, 0)
    ctx.getImageData(0, 0, img.width, img.height)
  }

  /** Get ImageData from a loaded Image, scaled to fit within maxW×maxH. Uses canvas drawImage scaling so we never
    * allocate huge buffers (avoids main-thread freeze on large uploads).
    */
  def imageToImageDataMaxSize(img: Image, maxW: Int, maxH: Int): ImageData = {
    val w = img.width
    val h = img.height
    if (w <= maxW && h <= maxH) imageToImageData(img)
    else {
      val scale    = (maxW.toDouble / w).min(maxH.toDouble / h)
      val nw       = (w * scale).toInt.max(1).min(maxW)
      val nh       = (h * scale).toInt.max(1).min(maxH)
      val (_, ctx) = createOffscreenCanvas(nw, nh)
      ctx.drawImage(img, 0, 0, w, h, 0, 0, nw, nh)
      ctx.getImageData(0, 0, nw, nh)
    }
  }

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
    val (_, ctx) = createOffscreenCanvas(raw.width, raw.height)
    val out      = ctx.createImageData(raw.width, raw.height)
    val dst      = out.data
    for (i <- raw.data.indices) dst(i) = raw.data(i) & 0xff
    out
  }
}
