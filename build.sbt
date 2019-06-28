import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val adjective =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalaVersion := "2.12.6",
      publish := {},
      publishLocal := {},
      libraryDependencies ++= List(
        "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
      )
    )

// PUBLISHING-RELATED

ThisBuild / name := "Adjective"
ThisBuild / version := "0.4.4"
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
    email = "me@victorivri.com",
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
