package com.acme

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

}
