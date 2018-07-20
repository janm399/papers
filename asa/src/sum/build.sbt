import sbt.Keys._

import scala.util.{Failure, Success, Try}

scalaVersion := "2.11.6"

organization := "com.acme"

import scala.sys.process._
libraryDependencies ++= (Try("nvidia-smi".!!) match {
  case Success(lines) if lines.contains("Driver Version") ⇒
    sLog.value.info("Will use CUDA ND4J back-end")
    Seq(
      "org.nd4j" % "nd4j-cuda-9.1-platform" % "1.0.0-beta" exclude ("org.bytedeco.javacpp-presets", "cuda"),
      "org.bytedeco.javacpp-presets" % "cuda-platform" % "9.1-7.1-1.4.1")
  case _ ⇒
    sLog.value.info("Will use native (CPU) ND4J back-end")
    Seq("org.nd4j" % "nd4j-native-platform" % "1.0.0-beta")
})

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.deeplearning4j" % "deeplearning4j-nlp" % "1.0.0-beta",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv" % "2.9.0",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.slf4j" % "slf4j-simple" % "1.7.12",
  "org.apache.commons" % "commons-text" % "1.4"
)
