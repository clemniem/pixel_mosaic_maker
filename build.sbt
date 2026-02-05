import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val pixel_mosaic_maker =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings( // Normal settings
      name         := "pixel_mosaic_maker",
      version      := "0.0.1",
      scalaVersion := "3.6.4",
      organization := "clemniem",
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "tyrian-io"   % "0.14.0",
        "io.circe"        %%% "circe-core"   % "0.14.6",
        "io.circe"        %%% "circe-parser" % "0.14.6",
        "io.circe"        %%% "circe-generic"% "0.14.6",
        "org.scalameta"   %%% "munit"        % "1.1.1" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
      scalafixOnCompile := true,
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision,
      autoAPIMappings   := true
    )
    .settings( // Welcome message
      logo := List(
        "",
        "Pixel Mosaic Maker (v" + version.value + ")",
        ""
      ).mkString("\n"),
      usefulTasks := Seq(
        UsefulTask("fastLinkJS", "Rebuild the JS (use during development)").noAlias,
        UsefulTask("fullLinkJS", "Rebuild the JS and optimise (use in production)").noAlias
      ),
      logoColor        := scala.Console.MAGENTA,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )
