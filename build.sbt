name := "reeds"
organization in ThisBuild := "com.acorns"
version in ThisBuild := "1.0.6"

scalaVersion in ThisBuild := "2.11.8"

libraryDependencies in ThisBuild ++= Seq(
  "org.typelevel" %% "cats" % "0.6.0",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)


lazy val `reeds-core` = project
lazy val `reeds-shapeless` = project dependsOn `reeds-core`
lazy val `reeds-circe` = project dependsOn `reeds-core`

lazy val reeds = (project in file(".")).aggregate(`reeds-core`, `reeds-shapeless`, `reeds-circe`)
