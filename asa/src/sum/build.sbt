import sbt.Keys._

scalaVersion := "2.12.6"

organization := "com.acme"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalanlp" % "nak" % "1.2.1" exclude ("org.apache.logging.log4j", "log4j-core"),
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv" % "2.9.0",
  "org.apache.commons" % "commons-text" % "1.4"
)
