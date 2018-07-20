package com.acme

import java.io._

import org.deeplearning4j.models.embeddings.loader.WordVectorSerializer
import org.deeplearning4j.models.paragraphvectors.ParagraphVectors
import org.deeplearning4j.text.documentiterator.{LabelAwareIterator, LabelledDocument, LabelsSource}
import org.deeplearning4j.text.tokenization.tokenizer.preprocessor.CommonPreprocessor
import org.deeplearning4j.text.tokenization.tokenizerfactory.DefaultTokenizerFactory

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait SourceClassifier {

  def classify(source: File): Seq[(String, Double)]

}

object SourceClassifier {

  import org.slf4j.LoggerFactory
  private val log = LoggerFactory.getLogger(SourceClassifier.getClass)

  def apply(exampleDirectory: Directory): SourceClassifier = {
    PVec(exampleDirectory)
  }

  object PVec {
    import scala.collection.JavaConverters._
    private val serializedFileName = "target/pv.ser"

    private class LDI(labelledDocuments: List[LabelledDocument], labelsSource: LabelsSource) extends LabelAwareIterator {
      private var iterator = labelledDocuments.iterator.asJava

      override def hasNextDocument: Boolean = iterator.hasNext
      override def nextDocument(): LabelledDocument = iterator.next()
      override def reset(): Unit = iterator = labelledDocuments.iterator.asJava
      override def getLabelsSource: LabelsSource = labelsSource
      override def shutdown(): Unit = ()
      override def hasNext: Boolean = iterator.hasNext
      override def next(): LabelledDocument = iterator.next()
    }

    def apply(exampleDirectory: Directory): PVec = {
      val classes = Seq(
        "akka", "akka-http", "akka-stream", "sbt", "actor", "backpressure", "scalatest", "scalacheck", "validation",
        "amazon-aws", "amazon-dynamo", "kafka", "amazon-kinesis", "amazon-s3", "protobuf", "concurrency", "future",
        "logging", "monitoring"
      )

      def train(exampleDirectory: Directory): ParagraphVectors = {
        def exampleToLabelledDocuments(row: ExampleLoader.Row): List[LabelledDocument] =
          row.sourceCodes().map { sc ⇒
            val doc = new LabelledDocument
            doc.setContent(sc)
            doc.setLabels(row.tags(classes).distinct.asJava)
            doc
          }

        log.debug("Loading examples...")
        val examples = ExampleLoader.loadLabels(exampleDirectory)
        log.debug(s"Loaded ${examples.length} examples")

        val labels = examples.flatMap(_.tags(classes)).distinct
        val labelsSource = new LabelsSource(labels.asJava)
        val labelledDocuments = examples.flatMap(exampleToLabelledDocuments)
        val iterator = new LDI(labelledDocuments, labelsSource)
        val tokenizerFactory = new DefaultTokenizerFactory
        log.debug(s"Starting training for labels $labels")

        tokenizerFactory.setTokenPreProcessor(new CommonPreprocessor)
        val paragraphVectors = new ParagraphVectors.Builder()
          .learningRate(0.025)
          .minLearningRate(0.001)
          .batchSize(1000)
          .epochs(20)
          .iterate(iterator)
          .trainWordVectors(true)
          .tokenizerFactory(tokenizerFactory)
          .build()
        paragraphVectors.fit()
        log.debug("Training finished")

        WordVectorSerializer.writeParagraphVectors(paragraphVectors, serializedFileName)

        paragraphVectors
      }

      def load(): Try[ParagraphVectors] = Try {
        WordVectorSerializer.readParagraphVectors(serializedFileName)
      }.flatMap { pv ⇒
        val pvLabels = pv.getLabelsSource.getLabels.asScala
        if (classes.forall(pvLabels.contains)) Success(pv)
        else Failure(new RuntimeException("Mismatched classes"))
      }

      val exampleDirectory = Directory("~/Downloads/so")
      val pv = load().getOrElse(train(exampleDirectory))
      new PVec(pv)
    }
  }

  class PVec(paragraphVectors: ParagraphVectors) extends SourceClassifier {

    override def classify(source: File): Seq[(String, Double)] = {
      import scala.collection.JavaConverters._

      val meansBuilder = new MeansBuilder(paragraphVectors.getLookupTable, paragraphVectors.getTokenizerFactory)
      val seeker = new LabelSeeker(paragraphVectors.getLookupTable, paragraphVectors.getLabelsSource.getLabels)
      val documentAsCentroid = meansBuilder.documentAsVector(Source.fromFile(source).mkString)
      val scores = seeker.getScores(documentAsCentroid)
      scores.asScala.map(x ⇒ x.getFirst → x.getSecond.toDouble).toList
    }

  }

}


object M {

  def main(args: Array[String]): Unit = {
    val classifier = SourceClassifier(Directory("~/Downloads/so"))

    Directory("~/Sandbox/adengine")
      .findAll(".scala")
      .map(f ⇒ f → classifier.classify(f))
      .foreach(println)
  }
}
