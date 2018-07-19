package com.acme

import java.io._

import nak.core.FeaturizedClassifier
import nak.data.{BowFeaturizer, Example}
import nak.liblinear.{LiblinearConfig, SolverType}
import org.apache.logging.log4j.LogManager

import scala.io.Source
import scala.util.{Random, Try}

trait SourceClassifier {

  def classify(source: File): Seq[(String, Double)]

}

object SourceClassifier {
  import nak.NakContext._
  private type Classifier = FeaturizedClassifier[String, String]
  private val log = LogManager.getLogger(SourceClassifier.getClass)
  private val serializedModelFileName = "target/classifier.ser"

  def train(exampleDirectory: Directory): Classifier = {
    val sources = ExampleLoader.loadLabels(exampleDirectory)
    log.info(s"Loaded ${sources.length} examples")

    val classes = Seq(
      "akka", "akka-http", "akka-stream", "sbt", "actor", "backpressure", "scalatest", "scalacheck", "validation",
      "amazon-aws", "amazon-dynamo", "kafka", "amazon-kinesis", "amazon-s3", "protobuf", "concurrency", "future",
      "logging", "monitoring"
    )
    val stopwords = Set("<-", "←", "<:", "<%", "=", "=>", "⇒", ">:", "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")
    val config = LiblinearConfig(cost = 10.0, eps = 0.01, solverType = SolverType.L2R_LR_DUAL, showDebug = true)
    val featurizer = new BowFeaturizer(stopwords)
    val trainingExamples = classes.flatMap { cls ⇒
      val t = sources.filter(_.isTagged(cls))
      Random.shuffle(t).flatMap(_.sourceCodes().map(Example(cls, _)))
    }

    log.info("Training...")
    val classifier = trainClassifier(config, featurizer, trainingExamples)
    val oos = new ObjectOutputStream(new FileOutputStream(serializedModelFileName))
    oos.writeObject(classifier)
    oos.close()
    log.info("Training completed.")
    classifier
  }

  def apply(alwaysTrain: Boolean): SourceClassifier = {
    def loadTrained(): Classifier = {
      val ois = new ObjectInputStream(new FileInputStream(serializedModelFileName))
      val classifier = ois.readObject().asInstanceOf[Classifier]
      ois.close()
      classifier
    }
    val exampleDirectory = Directory("/Users/janmachacek/Downloads/so")
    val classifier = if (alwaysTrain) train(exampleDirectory) else Try(loadTrained()).getOrElse(train(exampleDirectory))
    val minScore = 0.7

    new SourceClassifier {
      override def classify(source: File): Seq[(String, Double)] = {
        val contents = Source.fromFile(source, "UTF-8").mkString
        for {
          (score, idx) ← classifier.evalRaw(contents).zipWithIndex
          if score > minScore
        } yield (classifier.labelOfIndex(idx), score)
      }
    }
  }

}


object M {
  def main(args: Array[String]): Unit = {
    val classifier = SourceClassifier(false)

    Directory("/Users/janmachacek/Sandbox/adengine")
      .findAll(".scala")
      .map(f ⇒ f → classifier.classify(f))
      .foreach(println)
  }
}
