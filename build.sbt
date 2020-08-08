import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

addCommandAlias("testJVM", "adjectiveJVM/test")
addCommandAlias("testJS", "adjectiveJS/test")

lazy val adjective =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(
      crossScalaVersions := List("2.12.10", "2.13.1"),
      publish := {},
      publishLocal := {},
      libraryDependencies ++= List(
        "org.scalatest" %%% "scalatest" % "3.2.0" % "test"
      )
    )

lazy val adjectiveJVM = adjective.jvm
lazy val adjectiveJS  = adjective.js

// PUBLISHING-RELATED
inThisBuild(
  List(
    name := "Adjective",
    version := "0.5.0",
    description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective.",
    licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE")),
    homepage := Some(url("https://github.com/vivri/adjective")),

    organization := "com.victorivri",
    organizationName := "vivri",
    organizationHomepage := Some(url("http://victorivri.com/")),

    scmInfo := Some(
      ScmInfo(
        url("https://github.com/vivri/adjective"),
        "scm:git@github.com:vivri/adjective.git"
      )
    ),

    developers := List(
      Developer("vivri", "Victor Ivri", "me@victorivri.com", url("https://github.com/vivri")),
      Developer("mijicd", "Dejan Mijić", "dmijic@acm.org", url("https://github.com/mijicd"))
    ),

    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    publishMavenStyle := true
  )
)

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")

useGpg := true
