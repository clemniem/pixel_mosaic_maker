package mosaic.domain

final case class MosaicConfigId(value: String) extends AnyVal

final case class MosaicCreateConfig(
  id: MosaicConfigId,
  grid: GridConfig,
  palette: Palette,
  image: UploadedImage,
  offset: ImageOffset
)

final case class MosaicCell(
  row: Int,
  col: Int,
  color: ColorId
)

final case class MosaicPlan(
  configId: MosaicConfigId,
  cells: Vector[MosaicCell]
)
