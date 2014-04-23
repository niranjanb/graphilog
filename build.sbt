import com.typesafe.sbt.SbtNativePackager.packageArchetype

name := "graphilog"

description := "A framework that supports graph-based reasoning over knowledge extracted from text."

version := "0.1.0-SNAPSHOT"


libraryDependencies ++= Seq(
  "org.allenai.tef" %% "tef-common" % "0.9.0-SNAPSHOT",
  "org.allenai.common" %% "common" % "0.0.1-SNAPSHOT",
  "edu.wisc.hazy" %% "tuffy-internal" % "0.3.0",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "com.michaelpollmeier" % "gremlin-scala" % "2.4.1",
  "org.allenai.ari.solvers" %% "solvers-common" % "0.0.1-SNAPSHOT"
  ) ++ Seq(
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "com.jsuereth" %% "scala-arm" % "1.3" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.10.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-Xlint")

scalaVersion := "2.10.3"

Publish.settings ++ Format.settings ++ TravisPublisher.settings

resolvers ++= Seq(
  "spray" at "http://repo.spray.io",
  "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
  "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"
)

// SBT native packager configs.
packageArchetype.java_application

// Map conf => conf, src/main/scripts => bin
// See http://www.scala-sbt.org/0.12.3/docs/Detailed-Topics/Mapping-Files.html
// for more info on sbt mappings.
// this is insane
mappings in Universal ++=
  (sourceDirectory.value / "main" / "scripts" ** "*" x rebase(sourceDirectory.value / "main" / "scripts", "bin/")) ++
  (baseDirectory.value / "conf" ** "*" x rebase(baseDirectory.value / "conf", "conf/"))

