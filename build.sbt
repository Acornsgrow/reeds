name := "reeds"
organization in ThisBuild := "com.acorns"
version in ThisBuild := "1.0.7"

scalaVersion in ThisBuild := "2.11.8"

libraryDependencies in ThisBuild ++= Seq(
  "org.typelevel" %% "cats-jvm" % "0.7.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
)


lazy val `reeds-core` = project
lazy val `reeds-shapeless` = project dependsOn `reeds-core`
lazy val `reeds-circe` = project dependsOn `reeds-core`

lazy val reeds = (project in file(".")).aggregate(`reeds-core`, `reeds-shapeless`, `reeds-circe`)
