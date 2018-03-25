import sbt.Keys._

scalaVersion := "2.12.4"

organization := "com.acme"

resolvers += Resolver.bintrayRepo("cakesolutions", "maven")

PB.protoSources in Compile := Seq(file("../protobuf"))

// Hook up the PB generator task to the Compile task
managedSourceDirectories in Compile += target.value / "protobuf-generated"
PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true, singleLineToString = true) -> (target.value / "protobuf-generated")
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf",
  "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.3",

  // Evicted Guava replacement
  "com.google.guava" % "guava" % "23.0",

    // Kafka client
  "net.cakesolutions" %% "scala-kafka-client" % "1.0.0",
  "net.cakesolutions" %% "scala-kafka-client-akka" % "1.0.0",

  // Akka
  "com.typesafe.akka" %% "akka-actor" % "2.5.11",
  "com.typesafe.akka" %% "akka-stream" % "2.5.11",
  "com.typesafe.akka" %% "akka-http" % "10.1.0",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.0"
)

enablePlugins(DockerPlugin, AshScriptPlugin)

packageName in Docker := "com.acme.protocolchaos/chaos"

dockerUpdateLatest := true