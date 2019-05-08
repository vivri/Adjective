lazy val root = project.in(file("./library")).
  aggregate(js, jvm).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val library = crossProject.in(file("./library")).settings(
  scalaVersion := "2.12.6",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
).jsSettings(
  // JS-specific settings here
).jvmSettings(
  // JVM-specific settings here
)

lazy val js = library.js

lazy val jvm = library.jvm


// PUBLISHING-RELATED

ThisBuild / name := "Adjective"
ThisBuild / version := "0.4.1"
ThisBuild / description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective."
ThisBuild / licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/vivri/adjective"))

ThisBuild / organization := "com.victorivri"
ThisBuild / organizationName := "vivri"
ThisBuild / organizationHomepage := Some(url("http://victorivri.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/vivri/adjective"),
    "scm:git@github.com:vivri/adjective.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "vivri",
    name = "Victor Ivri",
    email = "victor.ivri@gmail.com",
    url = url("http://www.victorivri.com")
  )
)

ThisBuild / description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective."
ThisBuild / licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/vivri/adjective"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")

useGpg := true