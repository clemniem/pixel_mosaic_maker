package mosaic.domain

final case class ImageSize(width: Int, height: Int)

final case class UploadedImage(
  id: String,
  size: ImageSize
)

final case class ImageOffset(x: Int, y: Int)
