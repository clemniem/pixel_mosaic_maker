package clemniem.common.pdf

import clemniem.Color
import clemniem.common.pdf.Instruction.*
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

/** Lightweight renderer that draws a single PDF page onto a Canvas using [[Instruction]]s.
  *
  * Intended for *preview only* in the UI (PrintInstructionsScreen). It does not aim for perfect typographic fidelity;
  * it just needs to be representative and fast.
  */
object PdfPreviewRenderer {

  private val mmPerPt: Double = 25.4 / 72.0

  private def mmToPx(mm: Double, pxPerMm: Double): Double = mm * pxPerMm

  private def setFont(ctx: CanvasRenderingContext2D, fontSizePt: Int, pxPerMm: Double): Unit = {
    val px = (fontSizePt.toDouble * mmPerPt * pxPerMm).max(6.0)
    ctx.font = s"${px}px \"Press Start 2P\", monospace"
  }

  private def roundRectPath(
    ctx: CanvasRenderingContext2D,
    x: Double,
    y: Double,
    w: Double,
    h: Double,
    r: Double
  ): Unit = {
    val rr = r.max(0.0).min((w / 2).min(h / 2))
    ctx.beginPath()
    ctx.moveTo(x + rr, y)
    ctx.lineTo(x + w - rr, y)
    ctx.arcTo(x + w, y, x + w, y + rr, rr)
    ctx.lineTo(x + w, y + h - rr)
    ctx.arcTo(x + w, y + h, x + w - rr, y + h, rr)
    ctx.lineTo(x + rr, y + h)
    ctx.arcTo(x, y + h, x, y + h - rr, rr)
    ctx.lineTo(x, y + rr)
    ctx.arcTo(x, y, x + rr, y, rr)
    ctx.closePath()
  }

  private def renderPageNumberIfNeeded(
    ctx: CanvasRenderingContext2D,
    pageWmm: Double,
    pageHmm: Double,
    pageIndex0Based: Int,
    totalPages: Int,
    printerMarginMm: Double,
    pxPerMm: Double
  ): Unit =
    if (pageIndex0Based >= 2 && pageIndex0Based <= totalPages - 3) {
      val contentPageNum = pageIndex0Based - 1
      val leftSide       = (contentPageNum % 2) == 0
      setFont(ctx, fontSizePt = 9, pxPerMm)
      ctx.fillStyle = "rgb(0,0,0)"
      val yMm = pageHmm - printerMarginMm - 5
      val xMm = if (leftSide) printerMarginMm + 5 else pageWmm - printerMarginMm - 5
      ctx.textAlign = if (leftSide) "left" else "right"
      ctx.fillText(contentPageNum.toString, mmToPx(xMm, pxPerMm), mmToPx(yMm, pxPerMm))
    }

  def render(
    canvas: Canvas,
    instructions: List[Instruction],
    pageWmm: Double,
    pageHmm: Double,
    pageBackground: Color,
    printerMarginMm: Double,
    pageIndex0Based: Int,
    totalPages: Int
  ): Unit = {
    val wPx     = canvas.width.toDouble.max(1.0)
    val hPx     = canvas.height.toDouble.max(1.0)
    val pxPerMm = (wPx / pageWmm).min(hPx / pageHmm)

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    ctx.save()
    try {
      // Page base: white everywhere, then colored page background inside printer margin.
      ctx.globalAlpha = 1.0
      ctx.clearRect(0, 0, wPx, hPx)
      ctx.fillStyle = "rgb(255,255,255)"
      ctx.fillRect(0, 0, wPx, hPx)

      val mPx = mmToPx(printerMarginMm.max(0.0), pxPerMm)
      val bgX = mPx
      val bgY = mPx
      val bgW = (wPx - 2 * mPx).max(0.0)
      val bgH = (hPx - 2 * mPx).max(0.0)
      ctx.fillStyle = s"rgb(${pageBackground.r},${pageBackground.g},${pageBackground.b})"
      ctx.fillRect(bgX, bgY, bgW, bgH)

      ctx.textAlign = "left"
      ctx.textBaseline = "alphabetic"

      // Render instruction stream with an immutable font-size state.
      val _ = instructions.foldLeft(10) { (fontSizePt, inst) =>
        setFont(ctx, fontSizePt, pxPerMm)
        inst match {
          case PageSize(_, _) =>
            fontSizePt // page size is handled by caller / canvas dimensions

          case FontSize(pt) =>
            pt

          case Text(xMm, yMm, value) =>
            ctx.fillStyle = "rgb(0,0,0)"
            ctx.textAlign = "left"
            ctx.fillText(value, mmToPx(xMm, pxPerMm), mmToPx(yMm, pxPerMm))
            fontSizePt

          case TextAligned(xMm, yMm, value, align, fsPt) =>
            setFont(ctx, fsPt, pxPerMm)
            ctx.fillStyle = "rgb(0,0,0)"
            ctx.textAlign = align match {
              case "center" => "center"
              case "right"  => "right"
              case _        => "left"
            }
            ctx.fillText(value, mmToPx(xMm, pxPerMm), mmToPx(yMm, pxPerMm))
            setFont(ctx, fontSizePt, pxPerMm)
            fontSizePt

          case TextWithBackground(xMm, yTopMm, value, fsPt, paddingMm, alignLeft, bgR, bgG, bgB) =>
            setFont(ctx, fsPt, pxPerMm)
            val paddingPx = mmToPx(paddingMm, pxPerMm)
            val tw        = ctx.measureText(value).width
            val th        = (fsPt.toDouble * mmPerPt * pxPerMm).max(6.0)
            val boxW      = tw + 2 * paddingPx
            val boxH      = th + 2 * paddingPx
            val boxX      = if (alignLeft) mmToPx(xMm, pxPerMm) else mmToPx(xMm, pxPerMm) - boxW
            val boxY      = mmToPx(yTopMm, pxPerMm)
            ctx.fillStyle = s"rgb($bgR,$bgG,$bgB)"
            ctx.fillRect(boxX, boxY, boxW, boxH)
            ctx.fillStyle = "rgb(0,0,0)"
            ctx.textAlign = if (alignLeft) "left" else "right"
            val textX = if (alignLeft) boxX + paddingPx else boxX + boxW - paddingPx
            val textY = boxY + paddingPx + th
            ctx.fillText(value, textX, textY)
            setFont(ctx, fontSizePt, pxPerMm)
            fontSizePt

          case DrawLine(x1, y1, x2, y2, lineWidthMm, r, g, b) =>
            ctx.strokeStyle = s"rgb($r,$g,$b)"
            ctx.lineWidth = mmToPx(lineWidthMm, pxPerMm).max(0.5)
            ctx.beginPath()
            ctx.moveTo(mmToPx(x1, pxPerMm), mmToPx(y1, pxPerMm))
            ctx.lineTo(mmToPx(x2, pxPerMm), mmToPx(y2, pxPerMm))
            ctx.stroke()
            fontSizePt

          case DrawStrokeRects(rectsMm, r, g, b, lineWidthMm) =>
            ctx.strokeStyle = s"rgb($r,$g,$b)"
            ctx.lineWidth = mmToPx(lineWidthMm, pxPerMm).max(0.5)
            rectsMm.foreach { case (x, y, w, h) =>
              ctx.strokeRect(mmToPx(x, pxPerMm), mmToPx(y, pxPerMm), mmToPx(w, pxPerMm), mmToPx(h, pxPerMm))
            }
            fontSizePt

          case FillRect(x, y, w, h, r, g, b) =>
            ctx.globalAlpha = 1.0
            ctx.fillStyle = s"rgb($r,$g,$b)"
            ctx.fillRect(mmToPx(x, pxPerMm), mmToPx(y, pxPerMm), mmToPx(w, pxPerMm), mmToPx(h, pxPerMm))
            fontSizePt

          case FillRectWithOpacity(x, y, w, h, r, g, b, opacity) =>
            val a = opacity.max(0.0).min(1.0)
            ctx.save()
            ctx.globalAlpha = a
            ctx.fillStyle = s"rgb($r,$g,$b)"
            ctx.fillRect(mmToPx(x, pxPerMm), mmToPx(y, pxPerMm), mmToPx(w, pxPerMm), mmToPx(h, pxPerMm))
            ctx.restore()
            fontSizePt

          case RoundedFillRect(x, y, w, h, radius, r, g, b) =>
            val xp = mmToPx(x, pxPerMm)
            val yp = mmToPx(y, pxPerMm)
            val wp = mmToPx(w, pxPerMm)
            val hp = mmToPx(h, pxPerMm)
            val rp = mmToPx(radius, pxPerMm)
            ctx.fillStyle = s"rgb($r,$g,$b)"
            roundRectPath(ctx, xp, yp, wp, hp, rp)
            ctx.fill()
            fontSizePt

          case RoundedStrokeRect(x, y, w, h, radius, r, g, b, lineWidthMm) =>
            val xp = mmToPx(x, pxPerMm)
            val yp = mmToPx(y, pxPerMm)
            val wp = mmToPx(w, pxPerMm)
            val hp = mmToPx(h, pxPerMm)
            val rp = mmToPx(radius, pxPerMm)
            ctx.strokeStyle = s"rgb($r,$g,$b)"
            ctx.lineWidth = mmToPx(lineWidthMm, pxPerMm).max(0.5)
            roundRectPath(ctx, xp, yp, wp, hp, rp)
            ctx.stroke()
            fontSizePt

          case DrawSwatchRow(xMm, yMm, r, g, b, count, swatchSizeMm, gapMm, fsPt, strokeLineWidthMm) =>
            val x  = mmToPx(xMm, pxPerMm)
            val y  = mmToPx(yMm, pxPerMm)
            val sw = mmToPx(swatchSizeMm, pxPerMm)
            ctx.fillStyle = s"rgb($r,$g,$b)"
            ctx.fillRect(x, y, sw, sw)
            if (strokeLineWidthMm > 0) {
              ctx.strokeStyle = "rgb(0,0,0)"
              ctx.lineWidth = mmToPx(strokeLineWidthMm, pxPerMm).max(0.5)
              ctx.strokeRect(x, y, sw, sw)
            }
            setFont(ctx, fsPt, pxPerMm)
            ctx.fillStyle = "rgb(0,0,0)"
            ctx.textAlign = "left"
            val th        = (fsPt.toDouble * mmPerPt * pxPerMm).max(6.0)
            val baselineY = y + (sw + th) / 2
            ctx.fillText("×" + count.toString, x + sw + mmToPx(gapMm, pxPerMm), baselineY)
            setFont(ctx, fontSizePt, pxPerMm)
            fontSizePt

          case DrawPixelGrid(xMm, yMm, widthMm, heightMm, pixelWidth, pixelHeight, rgbFlat) =>
            if (pixelWidth > 0 && pixelHeight > 0 && rgbFlat.length >= pixelWidth * pixelHeight * 3) {
              val off = dom.document.createElement("canvas").asInstanceOf[Canvas]
              off.width = pixelWidth
              off.height = pixelHeight
              val octx = off.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
              val img  = octx.createImageData(pixelWidth, pixelHeight)
              (0 until (pixelWidth * pixelHeight)).foreach { p =>
                val i = p * 3
                val r = rgbFlat(i)
                val g = rgbFlat(i + 1)
                val b = rgbFlat(i + 2)
                val o = p * 4
                img.data(o) = r
                img.data(o + 1) = g
                img.data(o + 2) = b
                img.data(o + 3) = 255
              }
              octx.putImageData(img, 0, 0)
              ctx.imageSmoothingEnabled = false
              ctx.drawImage(
                off,
                mmToPx(xMm, pxPerMm),
                mmToPx(yMm, pxPerMm),
                mmToPx(widthMm, pxPerMm),
                mmToPx(heightMm, pxPerMm)
              )
              ctx.imageSmoothingEnabled = true
            }
            fontSizePt

          case AddPage =>
            fontSizePt // should not be present in per-page list

          case Save(_) =>
            fontSizePt // previews exclude Save
        }
      }

      // Page numbers are not part of the instruction list; JsPDF adds them at page boundaries.
      renderPageNumberIfNeeded(ctx, pageWmm, pageHmm, pageIndex0Based, totalPages, printerMarginMm, pxPerMm)
    } finally ctx.restore()
  }
}
