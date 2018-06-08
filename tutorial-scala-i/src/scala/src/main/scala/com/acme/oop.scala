package com.acme

import scala.util.Random

trait ReportGenerator {
  def generate(user: String): Array[Byte]
}

class ReportGeneratorImpl extends ReportGenerator with Cloneable {
  override def generate(user: String): Array[Byte] = Array.emptyByteArray
  override def clone(): AnyRef = ???
}

class ReportService(reportGenerator: ReportGenerator) {

  def reportAll(): Unit = {
    for (user ← Seq("a", "b", "c")) {
      reportGenerator.generate(user)
    }
  }
}

object Main extends App {

  val rs = new ReportService(new ReportGeneratorImpl)
  rs.reportAll()


  List(1, 2, 3).map(2 * _)

  Random.nextInt(10) match {
    case 0|1|2|3 ⇒
    case 6 ⇒
    case _ ⇒
  }

}
