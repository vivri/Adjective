
name := "Adjective"

version := "0.2"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

useGpg := true
credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")