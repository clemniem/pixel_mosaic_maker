package clemniem.common

import cats.effect.IO
import clemniem.{StorageKeys, StoredBuild, StoredBuildConfig, StoredImage, StoredPalette, StoredPrintConfig}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import org.scalajs.dom
import org.scalajs.dom.html.Input

import scala.scalajs.js

/** Self-contained export bundle: a print config with all entities it depends on. */
final case class PrintExportBundle(
  version: Int,
  printConfig: StoredPrintConfig,
  build: Option[StoredBuild],
  buildConfig: Option[StoredBuildConfig],
  image: Option[StoredImage],
  palette: Option[StoredPalette])

object PrintExportBundle {
  given Encoder[PrintExportBundle] = deriveEncoder
  given Decoder[PrintExportBundle] = deriveDecoder
}

object ExportImportUtils {

  private def loadListSync[A: Decoder](key: String): List[A] =
    Option(dom.window.localStorage.getItem(key))
      .flatMap(json => io.circe.parser.decode[List[A]](json).toOption)
      .getOrElse(Nil)

  private def saveListSync[A: Encoder](key: String, items: List[A]): Unit = {
    val json = items.asJson.noSpacesSortKeys
    dom.window.localStorage.setItem(key, json)
  }

  /** Build a [[PrintExportBundle]] by resolving all dependencies from LocalStorage. */
  def buildBundle(printConfig: StoredPrintConfig): PrintExportBundle = {
    val builds       = loadListSync[StoredBuild](StorageKeys.builds)
    val buildConfigs = loadListSync[StoredBuildConfig](StorageKeys.buildConfigs)
    val images       = loadListSync[StoredImage](StorageKeys.images)
    val palettes     = loadListSync[StoredPalette](StorageKeys.palettes)

    val build = printConfig.selectedBuildId.flatMap(id => builds.find(_.id == id))

    val buildConfigId = printConfig.selectedBuildConfigId
      .orElse(build.map(_.buildConfigRef))
    val buildConfig = buildConfigId.flatMap(id => buildConfigs.find(_.id == id))

    val image   = buildConfig.flatMap(bc => images.find(_.id == bc.config.imageRef))
    val palette = buildConfig.flatMap(bc => palettes.find(_.id == bc.config.paletteRef))

    PrintExportBundle(
      version = 1,
      printConfig = printConfig,
      build = build,
      buildConfig = buildConfig,
      image = image,
      palette = palette
    )
  }

  /** Export a print config as a downloadable JSON file. */
  def exportPrintConfig(printConfig: StoredPrintConfig): IO[Unit] = IO {
    val bundle   = buildBundle(printConfig)
    val json     = bundle.asJson.spaces2
    val filename = sanitizeFilename(printConfig.name) + ".json"

    val blob = js.Dynamic.newInstance(js.Dynamic.global.Blob)(
      js.Array(json),
      js.Dynamic.literal(`type` = "application/json")
    )
    val url = js.Dynamic.global.URL.createObjectURL(blob).asInstanceOf[String]
    val a   = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    a.href = url
    a.setAttribute("download", filename)
    a.style.display = "none"
    val _ = dom.document.body.appendChild(a)
    a.click()
    Option(a.parentNode).foreach(_.removeChild(a))
    val _ = js.Dynamic.global.URL.revokeObjectURL(url)
  }

  private def sanitizeFilename(name: String): String = {
    val cleaned = name.trim.replaceAll("[^a-zA-Z0-9._-]", "_")
    if (cleaned.isEmpty) "export" else cleaned
  }

  /** Open a file picker and import a [[PrintExportBundle]] JSON file. Returns the number of entities added. */
  def importPrintBundle(): IO[Int] =
    readJsonFile().flatMap { json =>
      io.circe.parser.decode[PrintExportBundle](json) match {
        case Left(err)     => IO.raiseError(new RuntimeException(s"Invalid export file: ${err.getMessage}"))
        case Right(bundle) => IO(mergeBundle(bundle))
      }
    }

  private def readJsonFile(): IO[String] =
    IO.async_[String] { cb =>
      val input = dom.document.createElement("input").asInstanceOf[Input]
      input.`type` = "file"
      input.accept = ".json,application/json"
      input.style.display = "none"
      val _ = dom.document.body.appendChild(input)
      def cleanup(): Unit = Option(input.parentNode).foreach(_.removeChild(input))
      input.addEventListener(
        "change",
        (_: dom.Event) => {
          val file = Option(input.files(0))
          input.value = ""
          cleanup()
          file match {
            case None => cb(Left(new RuntimeException("No file selected")))
            case Some(f) =>
              val reader = new dom.FileReader()
              reader.onload = _ => cb(Right(reader.result.toString))
              reader.onerror = _ => cb(Left(new RuntimeException("Failed to read file")))
              reader.readAsText(f)
          }
        }
      )
      input.click()
    }

  /** Merge bundle entities into LocalStorage. Returns the count of newly added entities. */
  private def mergeBundle(bundle: PrintExportBundle): Int = {
    val counts = List(
      mergeOne(StorageKeys.printConfigs, bundle.printConfig),
      bundle.build.map(b => mergeOne(StorageKeys.builds, b)).getOrElse(0),
      bundle.buildConfig.map(bc => mergeOne(StorageKeys.buildConfigs, bc)).getOrElse(0),
      bundle.image.map(img => mergeOne(StorageKeys.images, img)).getOrElse(0),
      bundle.palette.map(pal => mergeOne(StorageKeys.palettes, pal)).getOrElse(0)
    )
    counts.sum
  }

  /** Add entity to a list in LocalStorage if no entry with the same id exists. Returns 1 if added, 0 if skipped. */
  private def mergeOne[A: Encoder: Decoder](key: String, entity: A)(using idOf: HasId[A]): Int = {
    val existing = loadListSync[A](key)
    if (existing.exists(e => idOf.id(e) == idOf.id(entity))) 0
    else {
      saveListSync(key, entity :: existing)
      1
    }
  }

  /** Type class to extract an id from a stored entity without requiring a common trait bound in the generic merge. */
  trait HasId[A] { def id(a: A): String }
  object HasId {
    given HasId[StoredPrintConfig] with { def id(a: StoredPrintConfig): String = a.id }
    given HasId[StoredBuild] with       { def id(a: StoredBuild): String = a.id }
    given HasId[StoredBuildConfig] with  { def id(a: StoredBuildConfig): String = a.id }
    given HasId[StoredImage] with       { def id(a: StoredImage): String = a.id }
    given HasId[StoredPalette] with     { def id(a: StoredPalette): String = a.id }
  }
}
