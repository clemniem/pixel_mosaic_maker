package clemniem.common.pdf

/**
 * Layout configuration for the PDF book. All numeric layout choices (margins, paddings, font sizes, gaps)
 * are defined here so layout can be changed in one place without editing PdfUtils.
 * Aligned with docs/print_instructions_layout.md (cover, full overview, chapter overview, section overview, layer pages).
 */
final case class PdfLayoutConfig(
    global: PdfLayoutConfig.Global,
    cover: PdfLayoutConfig.Cover,
    fullOverview: PdfLayoutConfig.FullOverview,
    chapterOverview: PdfLayoutConfig.ChapterOverview,
    sectionOverview: PdfLayoutConfig.SectionOverview,
    layerPage: PdfLayoutConfig.LayerPage
)

object PdfLayoutConfig {

  /** Page size and content margins (used for all pages). */
  final case class Global(
      pageSizeMm: Double,
      contentPaddingLRMm: Double,
      contentPaddingTBMm: Double
  )

  /** Cover page: with mosaic (centered image, frame, title in corner) or title-only. */
  final case class Cover(
      titleFontSizePt: Int,
      titleOffsetYFromTopMm: Double,
      titleBlockHeightMm: Double,
      titleOnlyXMm: Double,
      titleOnlyYMm: Double,
      frameWhiteMarginMm: Double,
      frameStrokeLineWidthMm: Double,
      frameCornerRadiusMm: Double,
      titleBoxPaddingMm: Double
  )

  /** Full overview page: color list top-left, mosaic right (top-aligned with list). No title. Exploded view uses grid parts with gap between them. */
  final case class FullOverview(
      titleOffsetFromTopMm: Double,
      colorListReservedWidthMm: Double,
      explodedGapMm: Double,
      titleFontSizePt: Int,
      countLabelFontSizePt: Int,
      swatch: SwatchBlock
  )

  /** Shared swatch list block (full overview and chapter overview). Row = swatch (with black frame) + gap + "× count"; text centered with swatch. lineHeightMm > swatchSizeMm for row spacing. */
  final case class SwatchBlock(
      swatchSizeMm: Double,
      swatchGapMm: Double,
      lineHeightMm: Double,
      swatchStrokeLineWidthMm: Double,
      countFontSizePt: Int,
      firstLineOffsetMm: Double
  )

  /** Chapter (plate) overview page: small mosaic, plate image, colors for this plate. */
  final case class ChapterOverview(
      plateImageTopOffsetMm: Double,
      plateImageHeightReserveForScalePx: Double,
      smallOverviewHeightMm: Double,
      smallOverviewTopOffsetMm: Double,
      chapterTitleFontSizePt: Int,
      chapterTitleOffsetFromTopMm: Double,
      countLabelFontSizePt: Int,
      countListGapBelowImageMm: Double,
      divisibilityNoteFontSizePt: Int,
      divisibilityNoteOffsetAboveCountYMm: Double,
      nonDivisibleMessageFontSizePt: Int,
      nonDivisibleMessageOffsetFromTopMm: Double,
      swatch: SwatchBlock
  )

  /** Section overview: plate with step region highlighted. */
  final case class SectionOverview(
      titleFontSizePt: Int,
      titleOffsetFromTopMm: Double,
      subtitleFontSizePt: Int,
      subtitleOffsetFromTopMm: Double,
      stepHighlightLineWidthMm: Double
  )

  /** Layer patch pages: 2×2 grid of patches, labels below. */
  final case class LayerPage(
      patchMarginMm: Double,
      patchGapMm: Double,
      patchGridCols: Int,
      patchesPerPage: Int,
      contentTopOffsetMm: Double,
      titleFontSizePt: Int,
      titleOffsetFromTopMm: Double,
      labelOffsetBelowPatchMm: Double,
      labelFontSizePt: Int,
      grid16LineWidthMm: Double,
      grid4x4LineWidthMm: Double,
      gridStrokeGrey: Int
  )

  val default: PdfLayoutConfig = PdfLayoutConfig(
    global = Global(
      pageSizeMm = 200.0,
      contentPaddingLRMm = 15.0,
      contentPaddingTBMm = 15.0
    ),
    cover = Cover(
      titleFontSizePt = 10,
      titleOffsetYFromTopMm = 8.0,
      titleBlockHeightMm = 12.0,
      titleOnlyXMm = 25.0,
      titleOnlyYMm = 95.0,
      frameWhiteMarginMm = 4.0,
      frameStrokeLineWidthMm = 1.6,
      frameCornerRadiusMm = 0.5,
      titleBoxPaddingMm = 1.5
    ),
    fullOverview = FullOverview(
      titleOffsetFromTopMm = -1.0,
      colorListReservedWidthMm = 22.0,
      explodedGapMm = 3.0,
      titleFontSizePt = 12,
      countLabelFontSizePt = 10,
      swatch = SwatchBlock(
        swatchSizeMm = 4.5,
        swatchGapMm = 0.5,
        lineHeightMm = 6.0,
        swatchStrokeLineWidthMm = 0.2,
        countFontSizePt = 10,
        firstLineOffsetMm = 0.0
      )
    ),
    chapterOverview = ChapterOverview(
      plateImageTopOffsetMm = 55.0,
      plateImageHeightReserveForScalePx = 30.0,
      smallOverviewHeightMm = 45.0,
      smallOverviewTopOffsetMm = 6.0,
      chapterTitleFontSizePt = 12,
      chapterTitleOffsetFromTopMm = 4.0,
      countLabelFontSizePt = 10,
      countListGapBelowImageMm = 6.0,
      divisibilityNoteFontSizePt = 9,
      divisibilityNoteOffsetAboveCountYMm = 6.0,
      nonDivisibleMessageFontSizePt = 10,
      nonDivisibleMessageOffsetFromTopMm = 20.0,
      swatch = SwatchBlock(
        swatchSizeMm = 4.5,
        swatchGapMm = 0.5,
        lineHeightMm = 6.0,
        swatchStrokeLineWidthMm = 0.2,
        countFontSizePt = 10,
        firstLineOffsetMm = 5.0
      )
    ),
    sectionOverview = SectionOverview(
      titleFontSizePt = 12,
      titleOffsetFromTopMm = 4.0,
      subtitleFontSizePt = 10,
      subtitleOffsetFromTopMm = 12.0,
      stepHighlightLineWidthMm = 0.35
    ),
    layerPage = LayerPage(
      patchMarginMm = 15.0,
      patchGapMm = 5.0,
      patchGridCols = 2,
      patchesPerPage = 4,
      contentTopOffsetMm = 10.0,
      titleFontSizePt = 10,
      titleOffsetFromTopMm = 6.0,
      labelOffsetBelowPatchMm = 4.0,
      labelFontSizePt = 8,
      grid16LineWidthMm = 0.25,
      grid4x4LineWidthMm = 0.1,
      gridStrokeGrey = 120
    )
  )
}
