import sbt.Keys._

scalaVersion := "2.12.6"

organization := "com.acme"

// PB.protoSources in Compile := Seq(file("../protobuf"))

// managedSourceDirectories in Compile += target.value / "protobuf-generated"
// PB.targets in Compile := Seq(
//   scalapb.gen(flatPackage = true, singleLineToString = true) -> (target.value / "protobuf-generated")
// )

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalaz" %% "scalaz-core" % "7.2.24",
  "org.scalaz" %% "scalaz-effect" % "7.2.24"
  //"com.typesafe.akka" %% "akka-http" % "10.1.1",
  //"com.typesafe.akka" %% "akka-stream" % "2.5.11"
  //"org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf",
  // "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.3",
  //"org.scalacheck" %% "scalacheck" % "1.13.5"
)
