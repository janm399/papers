package com.acme

import java.io._
import java.util

import org.deeplearning4j.models.paragraphvectors.ParagraphVectors
import org.deeplearning4j.text.documentiterator.{LabelAwareIterator, LabelledDocument, LabelsSource}
import org.deeplearning4j.text.tokenization.tokenizer.preprocessor.CommonPreprocessor
import org.deeplearning4j.text.tokenization.tokenizerfactory.DefaultTokenizerFactory

import scala.io.Source
import scala.util.Try

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
      val classes = List(
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
          .batchSize(100000)
          .epochs(20)
          .iterate(iterator)
          .stopWords(util.Arrays.asList("<-", "←", "<:", "<%", "=", "=>", "⇒", ">:", "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield"))
          .allowParallelTokenization(true)
          .trainWordVectors(true)
          .tokenizerFactory(tokenizerFactory)
          .build()
        paragraphVectors.fit()
        log.debug("Training finished")

        val oos = new ObjectOutputStream(new FileOutputStream(serializedFileName))
        oos.writeObject(paragraphVectors)
        oos.close()
//        WordVectorSerializer.writeParagraphVectors(paragraphVectors, serializedFileName)

        paragraphVectors
      }

      def load(): Try[ParagraphVectors] = Try {
        val ois = new ObjectInputStream(new FileInputStream(serializedFileName))
        val classifier = ois.readObject().asInstanceOf[ParagraphVectors]
        ois.close()
        classifier
      }
//        try {
//          val pv = WordVectorSerializer.readParagraphVectors(serializedFileName)
//          Success(pv)
//        } catch {
//          case t: Throwable ⇒
//            t.printStackTrace()
//            Failure(t)
//        }
//
//        try {
//          val ois = new ObjectInputStream(new FileInputStream(serializedFileName))
//          val classifier = ois.readObject().asInstanceOf[ParagraphVectors]
//          ois.close()
//          //val pvLabels = classifier.getLabelsSource.getLabels.asScala
//          Success(classifier)
//          //if (classes.forall(pvLabels.contains)) Success(classifier)
//          //else throw new RuntimeException("")
//        } catch {
//          case t: Throwable ⇒
//            t.printStackTrace()
//            Failure(t)
//        }
//      }

      val exampleDirectory = Directory("~/Downloads/so")
      val pv = load().getOrElse(train(exampleDirectory))
      new PVec(pv)
    }
  }

  class PVec(paragraphVectors: ParagraphVectors) extends SourceClassifier {

    override def classify(source: File): Seq[(String, Double)] = {
      import scala.collection.JavaConverters._

      val meansBuilder = new MeansBuilder(paragraphVectors.getLookupTable, new DefaultTokenizerFactory)
      val seeker = new LabelSeeker(paragraphVectors.getLookupTable, paragraphVectors.getLabelsSource.getLabels)
      try {
        val documentAsCentroid = meansBuilder.documentAsVector(Source.fromFile(source).mkString)
        val scores = seeker.getScores(documentAsCentroid)
        scores.asScala.map(x ⇒ x.getFirst → x.getSecond.toDouble).toList
      } catch {
        case t: Throwable ⇒
          println(source)
          t.printStackTrace()
          Nil
      }
    }

  }

}


object M {

  def main(args: Array[String]): Unit = {
    NDInitializer.init()
    val classifier = SourceClassifier(Directory("~/Downloads/so"))

    Directory("~/Sandbox")
      .findAll(".scala")
      .map(f ⇒ f → classifier.classify(f))
      .foreach(println)
  }
}
