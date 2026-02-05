package clemniem.common.pdf

import clemniem.{Color, GridConfig, PixelPic}

/** Pure SVG generation for the mosaic (pixel image + plate grid). Used for PDF overview. */
object MosaicSvg {

  /** SVG with viewBox 0 0 pic.width pic.height: one &lt;rect&gt; per pixel, then plate grid overlay.
    * Dimensions are in pixel units; scale when embedding (e.g. in PDF via width/height).
    */
  def mosaicToSvg(pic: PixelPic, grid: GridConfig): String = {
    val w = pic.width
    val h = pic.height
    val pixelRects = (0 until h).flatMap { y =>
      (0 until w).map { x =>
        val idx = pic.pixels(y * w + x)
        val px  = pic.paletteLookup(idx)
        val fill = Color(px.r, px.g, px.b).toHex
        s"""<rect x="$x" y="$y" width="1" height="1" fill="$fill"/>"""
      }
    }
    val gridRects = grid.parts.map { part =>
      s"""<rect x="${part.x}" y="${part.y}" width="${part.width}" height="${part.height}" fill="none" stroke="rgba(255,0,0,0.7)" stroke-width="0.5"/>"""
    }
    val body = (pixelRects ++ gridRects).mkString("\n")
    s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 $w $h" width="$w" height="$h">$body</svg>"""
  }
}
