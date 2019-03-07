ThisBuild / name := "Adjective"
ThisBuild / version := "0.3"
ThisBuild / description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective."
ThisBuild / homepage := Some(url("https://github.com/vivri/adjective"))
ThisBuild / licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE"))

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"