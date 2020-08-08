import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

addCommandAlias("testJVM", "adjectiveJVM/test")
addCommandAlias("testJS", "adjectiveJS/test")

inThisBuild(
  List(
    name := "Adjective",
    description := "Programming is an exercise in linguistics; spice-up Scala types with Adjective.",
    licenses := List("MIT" -> new URL("https://github.com/vivri/Adjective/blob/master/LICENSE")),
    homepage := Some(url("https://github.com/vivri/adjective")),
    organization := "com.victorivri",
    organizationName := "vivri",
    organizationHomepage := Some(url("http://victorivri.com/")),
    scmInfo := Some(ScmInfo(url("https://github.com/vivri/adjective"), "scm:git@github.com:vivri/adjective.git")),
    developers := List(
      Developer("vivri", "Victor Ivri", "me@victorivri.com", url("https://github.com/vivri")),
      Developer("mijicd", "Dejan Mijić", "dmijic@acm.org", url("https://github.com/mijicd"))
    ),
    pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc") 
  )
)

lazy val adjective =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(
      crossScalaVersions := List("2.12.10", "2.13.1"),
      libraryDependencies ++= List(
        "org.scalatest" %%% "scalatest" % "3.2.0" % Test
      )
    )

lazy val adjectiveJVM = adjective.jvm
lazy val adjectiveJS  = adjective.js
