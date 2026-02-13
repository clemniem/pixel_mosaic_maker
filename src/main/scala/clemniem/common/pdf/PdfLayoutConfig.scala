package clemniem.common.pdf

/**
 * Layout configuration for the PDF book. All numeric layout choices (margins, paddings, font sizes, gaps)
 * are defined here so layout can be changed in one place without editing PdfUtils.
 * Aligned with docs/print_instructions_layout.md (cover, full overview, chapter overview, layer pages).
 */
final case class PdfLayoutConfig(
    global: PdfLayoutConfig.Global,
    cover: PdfLayoutConfig.Cover,
    fullOverview: PdfLayoutConfig.FullOverview,
    chapterOverview: PdfLayoutConfig.ChapterOverview,
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

  /** Full overview page: color list top-left, mosaic right (top-aligned with list). No title. Exploded view uses grid parts with gap; dimension markings (architecture-style) on top and left. */
  final case class FullOverview(
      titleOffsetFromTopMm: Double,
      colorListReservedWidthMm: Double,
      explodedGapMm: Double,
      explodedDimensionGapMm: Double,
      explodedDimensionFontSizePt: Int,
      explodedDimensionLineWidthMm: Double,
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

  /** Chapter (section) overview page: same pattern as full overview – left = color list + small grid overview below; right = exploded section with dimension lines. No title. */
  final case class ChapterOverview(
      contentTopOffsetFromTopMm: Double,
      colorListReservedWidthMm: Double,
      gridOverviewMaxHeightMm: Double,
      gridOverviewGapAboveMm: Double,
      gridOverviewLeftMarginMm: Double,
      gridOverviewRightMarginMm: Double,
      gridOverviewExplodedGapMm: Double,
      explodedGapMm: Double,
      /** Scale factor for right-side exploded area (e.g. 0.85 = a bit smaller). */
      explodedAreaScaleFactor: Double,
      explodedDimensionGapMm: Double,
      explodedDimensionFontSizePt: Int,
      explodedDimensionLineWidthMm: Double,
      divisibilityNoteFontSizePt: Int,
      divisibilityNoteOffsetAboveCountYMm: Double,
      nonDivisibleMessageFontSizePt: Int,
      nonDivisibleMessageOffsetFromTopMm: Double,
      swatch: SwatchBlock
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
      explodedDimensionGapMm = 3.0,
      explodedDimensionFontSizePt = 8,
      explodedDimensionLineWidthMm = 0.5,
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
      contentTopOffsetFromTopMm = -1.0,
      colorListReservedWidthMm = 26.0,
      gridOverviewMaxHeightMm = 48.0,
      gridOverviewGapAboveMm = 6.0,
      gridOverviewLeftMarginMm = 3.0,
      gridOverviewRightMarginMm = 2.0,
      gridOverviewExplodedGapMm = 1.5,
      explodedGapMm = 3.0,
      explodedAreaScaleFactor = 0.85,
      explodedDimensionGapMm = 3.0,
      explodedDimensionFontSizePt = 8,
      explodedDimensionLineWidthMm = 0.5,
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
        firstLineOffsetMm = 0.0
      )
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
