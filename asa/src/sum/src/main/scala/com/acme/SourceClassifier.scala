package com.acme

import java.io._
import java.util

import org.deeplearning4j.models.paragraphvectors.ParagraphVectors
import org.deeplearning4j.text.documentiterator.{LabelAwareIterator, LabelledDocument, LabelsSource}
import org.deeplearning4j.text.tokenization.tokenizerfactory.DefaultTokenizerFactory

import scala.io.Source
import scala.util.Try

trait SourceClassifier {

  def classes: List[String]

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
        "amazon-dynamo", "kafka", "amazon-kinesis", "protobuf", "concurrency", "future", "logging", "monitoring"
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
        // tokenizerFactory.setTokenPreProcessor(new CommonPreprocessor)

        val paragraphVectors = new ParagraphVectors.Builder()
          .learningRate(0.025)
          .minLearningRate(0.001)
          .batchSize(100000)
          .epochs(20)
          .iterate(iterator)
          .layerSize(500)
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

        paragraphVectors
      }

      def load(): Try[ParagraphVectors] = Try {
        val ois = new ObjectInputStream(new FileInputStream(serializedFileName))
        val classifier = ois.readObject().asInstanceOf[ParagraphVectors]
        ois.close()
        val classifierClasses = classifier.getLabelsSource.getLabels.asScala
        if (classes.size == classifierClasses.size && classes.forall(classifierClasses.contains)) classifier
        else {
          log.warn("Mismatched classes; will re-train.")
          throw new RuntimeException("Mismatched classes")
        }
      }

      val exampleDirectory = Directory("~/Downloads/so")
      val pv = load().getOrElse(train(exampleDirectory))
      new PVec(pv, classes)
    }
  }

  class PVec(paragraphVectors: ParagraphVectors, override val classes: List[String]) extends SourceClassifier {

    override def classify(source: File): Seq[(String, Double)] = {
      def strip(source: File): String = Source
        .fromFile(source)
        .getLines()
        .filterNot(_.startsWith("package"))
        .filterNot(_.startsWith("import"))
        .filterNot(_.startsWith("//"))
        .filterNot(_.isEmpty)
        .mkString("\n")

      import scala.collection.JavaConverters._

      val meansBuilder = new MeansBuilder(paragraphVectors.getLookupTable, new DefaultTokenizerFactory)
      val seeker = new LabelSeeker(paragraphVectors.getLookupTable, paragraphVectors.getLabelsSource.getLabels)
      try {
        val strippedContent = strip(source)
        val documentAsCentroid = meansBuilder.documentAsVector(strippedContent)
        val scores = seeker.getScores(documentAsCentroid)
        scores.asScala.map(x ⇒ x.getFirst → x.getSecond.toDouble).toList
      } catch {
        case t: Throwable ⇒
          println("Skipping " + source)
          Nil
      }
    }

  }

}


object M {

  def main(args: Array[String]): Unit = {
    NDInitializer.init()
    val separator = ","

    def out(sourceDirectory: Directory, allClasses: List[String], bw: BufferedWriter)(f: File, classes: Seq[(String, Double)]): Unit = {
      val row = allClasses.map(c ⇒ classes.find(_._1 == c).map(_._2).getOrElse(0.0))
      if (row.exists(_ != 0)) {
        bw.append(sourceDirectory.fileName).append(separator)
        bw.append(sourceDirectory.relativeFileName(f)).append(separator)
        bw.append(row.mkString(separator))
        bw.append("\n")
      }
    }

    val sourceClassifier = SourceClassifier(Directory("~/Downloads/so"))
    val allClasses = sourceClassifier.classes

    val bw = new BufferedWriter(new FileWriter("target/out.csv"))
    bw.append("project").append(separator).append("file").append(separator)
    bw.append(allClasses.mkString(separator))
    bw.append("\n")

    Directory("~/Sandbox/asa-data/projects")
      .findDirectories()
      .foreach { sourceDirectory ⇒
        sourceDirectory
          .exclude("target")
          .findFilesR(".scala")
          .foreach { f ⇒
            val sourceClasses = sourceClassifier.classify(f)
            out(sourceDirectory, allClasses, bw)(f, sourceClasses)
          }
      }
    bw.close()

  }
}
