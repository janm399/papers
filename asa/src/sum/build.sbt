import sbt.Keys._

scalaVersion := "2.11.6"

organization := "com.acme"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.nd4j" % "nd4j-cuda-9.1-platform" % "1.0.0-beta" exclude ("org.bytedeco.javacpp-presets", "cuda"),
  "org.deeplearning4j" % "deeplearning4j-nlp" % "1.0.0-beta",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv" % "2.9.0",
  "org.bytedeco.javacpp-presets" % "cuda-platform" % "9.1-7.1-1.4.1",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.slf4j" % "slf4j-simple" % "1.7.12",
  "org.apache.commons" % "commons-text" % "1.4"
)
