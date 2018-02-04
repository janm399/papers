import sbt.Keys._

scalaVersion := "2.12.4"

organization := "com.bamtech"

PB.protoSources in Compile := Seq((sourceDirectory in Compile).value / "protobuf")

// Hook up the PB generator task to the Compile task
managedSourceDirectories in Compile += target.value / "protobuf-generated"
PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true, singleLineToString = true) -> (target.value / "protobuf-generated")
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf",
  "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.3",
  "org.scalacheck" %% "scalacheck" % "1.13.5",
  "com.google.guava" % "guava" % "23.0"
)
