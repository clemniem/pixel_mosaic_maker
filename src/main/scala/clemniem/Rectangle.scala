package clemniem

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class Rectangle(x: Int, y: Int, width: Int, height: Int) {
  def left: Int   = x
  def right: Int  = x + width
  def top: Int    = y
  def bottom: Int = y + height

  def clipTo(outer: Rectangle): Rectangle = {
    val cx = x.max(outer.x)
    val cy = y.max(outer.y)
    val cRight  = right.min(outer.right)
    val cBottom = bottom.min(outer.bottom)
    val cw = (cRight - cx).max(0)
    val ch = (cBottom - cy).max(0)
    Rectangle(cx, cy, cw, ch)
  }
}

object Rectangle {
  given Encoder[Rectangle] = deriveEncoder
  given Decoder[Rectangle] = deriveDecoder
}
