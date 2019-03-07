ThisBuild / name := "Adjective"
ThisBuild / version := "0.3"
ThisBuild / description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective."
ThisBuild / licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/vivri/adjective"))

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

useGpg := true
credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")