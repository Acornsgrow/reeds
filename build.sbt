name := "reeds"
organization in ThisBuild := "com.acorns"
version in ThisBuild := "1.0.8"

scalaVersion in ThisBuild := "2.11.8"
crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1")

libraryDependencies in ThisBuild ++= Seq(
  "org.typelevel" %% "cats-jvm" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)


lazy val `reeds-core` = project
lazy val `reeds-shapeless` = project dependsOn `reeds-core`
lazy val `reeds-circe` = project dependsOn `reeds-core`

lazy val reeds = (project in file(".")).aggregate(`reeds-core`, `reeds-shapeless`, `reeds-circe`)
