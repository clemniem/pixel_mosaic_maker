package clemniem.screens

import cats.effect.IO
import clemniem.*
import io.circe.{Decoder, Encoder}
import io.circe.syntax.*
import org.scalajs.dom

/** Sample data for all six galleries. Called from the About screen "Load sample data" button. */
object SeedData {

  /** Write sample data to localStorage. Returns the number of new items added (skips existing IDs). */
  def seed(): IO[Int] = IO.delay {
    List(
      seedKey[StoredPalette](StorageKeys.palettes, palettes),
      seedKey[StoredImage](StorageKeys.images, images),
      seedKey[StoredLayout](StorageKeys.layouts, layouts),
      seedKey[StoredBuildConfig](StorageKeys.buildConfigs, buildConfigs),
      seedKey[StoredBuild](StorageKeys.builds, builds),
      seedKey[StoredPrintConfig](StorageKeys.printConfigs, printConfigs)
    ).sum
  }

  private def seedKey[A <: StoredEntity](key: String, items: List[A])(using Encoder[List[A]], Decoder[List[A]]): Int = {
    val existing = Option(dom.window.localStorage.getItem(key))
      .flatMap(json => io.circe.parser.decode[List[A]](json).toOption)
      .getOrElse(Nil)
    val existingIds = existing.map(_.id).toSet
    val toAdd       = items.filterNot(a => existingIds.contains(a.id))
    if (toAdd.nonEmpty) {
      val merged = toAdd ++ existing
      dom.window.localStorage.setItem(key, merged.asJson.noSpacesSortKeys)
    }
    toAdd.size
  }

  // ---------------------------------------------------------------------------
  // Pixel art generators
  // ---------------------------------------------------------------------------

  /** 32x32 smiley face. Palette: 0=white, 1=yellow, 2=brown, 3=pink. */
  private def makeSmilePixels(): Vector[Int] = {
    val cx = 15.5; val cy = 15.5; val r2 = 12.5 * 12.5
    (for {
      y <- 0 until 32
      x <- 0 until 32
    } yield {
      val dx         = x - cx; val dy = y - cy
      val inFace     = (dx * dx + dy * dy) <= r2
      val leftEye    = (x >= 10 && x <= 11) && (y >= 11 && y <= 12)
      val rightEye   = (x >= 20 && x <= 21) && (y >= 11 && y <= 12)
      val leftCheek  = inFace && (x >= 6 && x <= 9) && (y >= 17 && y <= 18)
      val rightCheek = inFace && (x >= 22 && x <= 25) && (y >= 17 && y <= 18)
      val smile      = inFace && (((x == 9 || x == 10) && (y >= 20 && y <= 21)) ||
        ((x == 21 || x == 22) && (y >= 20 && y <= 21)) ||
        ((x >= 11 && x <= 20) && (y >= 22 && y <= 23)))
      if (!inFace) 0
      else if (leftEye || rightEye) 2
      else if (smile) 2
      else if (leftCheek || rightCheek) 3
      else 1
    }).toVector
  }

  /** 32x32 quilt pattern. Palette: 0=white, 1=red, 2=blue, 3=green, 4=gold. */
  private def makeQuiltPixels(): Vector[Int] = {
    val tc = Vector(Vector(1, 2, 3, 4), Vector(2, 3, 4, 1), Vector(3, 4, 1, 2), Vector(4, 1, 2, 3))
    (
      for {
        y <- 0 until 32
        x <- 0 until 32
      } yield if (x % 8 == 0 || y % 8 == 0) 0 else tc(y / 8)(x / 8)
    ).toVector
  }

  private def mkPic(w: Int, h: Int, palette: Vector[Pixel], pixels: Vector[Int], name: String): Option[PixelPic] = {
    val counts = pixels.groupBy(identity).view.mapValues(_.size).toMap
    PixelPic(w, h, palette, pixels, counts, name)
  }

  // ---------------------------------------------------------------------------
  // Sample entities
  // ---------------------------------------------------------------------------

  private val smileyPalette = Vector(
    Pixel(255, 255, 255, 255), // 0 background
    Pixel(255, 210, 30, 255),  // 1 face
    Pixel(45, 25, 5, 255),     // 2 features
    Pixel(255, 150, 150, 255)  // 3 cheeks
  )

  private val quiltPalette = Vector(
    Pixel(255, 255, 255, 255), // 0 grid lines
    Pixel(200, 45, 45, 255),   // 1 red
    Pixel(45, 95, 200, 255),   // 2 blue
    Pixel(45, 165, 95, 255),   // 3 green
    Pixel(255, 200, 0, 255)    // 4 gold
  )

  private lazy val smileyPic: Option[PixelPic] = mkPic(32, 32, smileyPalette, makeSmilePixels(), "Sample: Smiley Face")
  private lazy val quiltPic: Option[PixelPic]  = mkPic(32, 32, quiltPalette, makeQuiltPixels(), "Sample: Quilt Pattern")

  private val palettes: List[StoredPalette] = List(
    StoredPalette("sample-palette-smiley", "Sample: Smiley Colors", smileyPalette.map(p => Color(p.r, p.g, p.b))),
    StoredPalette("sample-palette-quilt", "Sample: Quilt Colors", quiltPalette.map(p => Color(p.r, p.g, p.b)))
  )

  private lazy val images: List[StoredImage] = List(
    smileyPic.map(pic => StoredImage("sample-image-smiley", "Sample: Smiley Face", pic)),
    quiltPic.map(pic => StoredImage("sample-image-quilt", "Sample: Quilt Pattern", pic))
  ).flatten

  private val singlePanel: Layout = Layout(1, 1, Array(GridPart(0, 0, 32, 32)))
  private val grid2x2: Layout     = Layout(
    2,
    2,
    Array(GridPart(0, 0, 16, 16), GridPart(16, 0, 16, 16), GridPart(0, 16, 16, 16), GridPart(16, 16, 16, 16)))

  private val layouts: List[StoredLayout] = List(
    StoredLayout("sample-layout-single", "Sample: Single Panel", singlePanel, None, None, None),
    StoredLayout("sample-layout-2x2", "Sample: 2\u00d72 Grid", grid2x2, None, None, None)
  )

  private val buildConfigs: List[StoredBuildConfig] = List(
    StoredBuildConfig(
      "sample-buildconfig-smiley",
      "Sample: Smiley Mosaic",
      BuildConfig(singlePanel, "sample-image-smiley", "sample-palette-smiley", 0, 0),
      None),
    StoredBuildConfig(
      "sample-buildconfig-quilt",
      "Sample: Quilt Mosaic",
      BuildConfig(grid2x2, "sample-image-quilt", "sample-palette-quilt", 0, 0),
      None)
  )

  private val builds: List[StoredBuild] = List(
    StoredBuild("sample-build-smiley", "Sample: Smiley Build", "sample-buildconfig-smiley", None, None),
    StoredBuild("sample-build-quilt", "Sample: Quilt Build", "sample-buildconfig-quilt", None, None)
  )

  private val printConfigs: List[StoredPrintConfig] = List(
    StoredPrintConfig(
      id = "sample-printconfig-quilt",
      name = "Sample: Quilt Print",
      selectedBuildId = Some("sample-build-quilt"),
      selectedBuildConfigId = Some("sample-buildconfig-quilt"),
      title = "My First Quilt Mosaic",
      stepSizePx = 16,
      pageBackgroundColorHex = "#fdfbe6",
      patchBackgroundColorHex = "#dcdcdc",
      stacked = true,
      printerMarginMm = 3.0,
      contentTopOffsetMm = 2.0
    )
  )
}
