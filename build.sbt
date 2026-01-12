ThisBuild / scalaVersion := "3.3.1"

lazy val root =
  project
    .in(file("."))
    .enablePlugins(org.scalajs.sbtplugin.ScalaJSPlugin)
    .settings(
      name := "mosaic-scala-js",
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "tyrian" % "0.14.0"
      )
    )
