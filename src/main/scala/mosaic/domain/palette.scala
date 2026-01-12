package mosaic.domain

final case class ColorId(value: String) extends AnyVal

final case class Rgba(r: Int, g: Int, b: Int, a: Int = 255)

final case class ColorDef(
  id: ColorId,
  name: String,
  rgba: Rgba
)

final case class Palette(colors: Vector[ColorId])

final case class PartTypeId(value: String) extends AnyVal

final case class Inventory(
  stock: Map[PartTypeId, Map[ColorId, Int]]
)
