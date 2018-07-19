package com.acme

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import com.fasterxml.jackson.databind.MappingIterator
import com.fasterxml.jackson.dataformat.csv.{CsvMapper, CsvSchema}
import org.apache.commons.text.StringEscapeUtils

object SourceTrainer {

  final case class Row(qTitle: String,
                       title: String,
                       creationDate: Date, score: Int, viewCount: Int, body: String,
                       tags: List[String],
                       ownerDisplayName: String) {

    def isTagged(tag: String): Boolean = qTitle.toLowerCase.contains(tag)

    def sourceCodes(): List[String] = {
      def i(fromIndex: Int): List[String] = {
        val start = body.indexOf("<pre><code>", fromIndex)
        if (start != -1) {
          val end = body.indexOf("</code></pre>", start)
          if (end != -1) {
            StringEscapeUtils.unescapeHtml4(body.substring(start + 11, end)) :: i(end)
          } else Nil
        } else Nil
      }

      i(0)
    }

  }

  object Row {
    val fmt = new SimpleDateFormat("dd-MM-yyyy hh:mm:ss")
    def apply(map: java.util.Map[String, String]): Row = {
      val vcs = map.get("ViewCount")
      val vc = if (vcs.isEmpty) 0 else vcs.toInt
      Row(map.get("QTitle"), map.get("Title"), fmt.parse(map.get("CreationDate")),
        map.get("Score").toInt, vc, map.get("Body"),
        map.get("Tags").split(",").toList,
        map.get("OwnerDisplayName")
      )
    }
  }

  def x(): List[Row] = {
    import scala.collection.JavaConverters._
    val schema = CsvSchema.emptySchema().withHeader()
    val csvFile = new File("/Users/janmachacek/Downloads/QueryResults.csv")
    val mapper = new CsvMapper()
    val jcontents: MappingIterator[java.util.Map[String, String]] =
      mapper.readerFor(classOf[java.util.Map[_, _]]).`with`(schema).readValues(csvFile)
    val contents = jcontents.asScala.map(Row.apply).toList
    contents
  }

  def main(array: Array[String]): Unit = {
    val y = x().filter(_.isTagged("akka")).map(_.sourceCodes())
    println(y)
  }

}
