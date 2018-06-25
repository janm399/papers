import sbt.Keys._

scalaVersion := "2.12.6"

organization := "com.acme"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalaz" %% "scalaz-core" % "7.2.24",
  "org.scalaz" %% "scalaz-effect" % "7.2.24",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)
