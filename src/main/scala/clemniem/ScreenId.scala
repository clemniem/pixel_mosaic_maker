package clemniem

/** Identifies a screen in the SPA. Scala 3 enum enables exhaustive matching. */
enum ScreenId(val name: String, val title: String) {
  case OverviewId          extends ScreenId("overview", "Overview")
  case LayoutsId           extends ScreenId("grid-configs", "Layout")
  case LayoutId            extends ScreenId("grid-config", "Edit layout")
  case PalettesId          extends ScreenId("palettes", "Palettes")
  case PaletteId           extends ScreenId("palette", "Palette")
  case ImagesId            extends ScreenId("images", "Images")
  case BuildConfigsId      extends ScreenId("build-configs", "Mosaic setup")
  case BuildsId            extends ScreenId("builds", "Build")
  case ImageUploadId       extends ScreenId("image-upload", "Upload image")
  case BuildConfigId       extends ScreenId("build-config", "Mosaic setup")
  case BuildId             extends ScreenId("build", "Building steps")
  case PrintConfigsId      extends ScreenId("print-configs", "Print")
  case PrintInstructionsId extends ScreenId("print-instructions", "Print")
  case AboutId             extends ScreenId("about", "About")
}
