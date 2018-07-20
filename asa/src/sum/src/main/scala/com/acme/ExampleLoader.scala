package com.acme

import java.text.SimpleDateFormat
import java.util.Date

import com.fasterxml.jackson.databind.MappingIterator
import com.fasterxml.jackson.dataformat.csv.{CsvMapper, CsvSchema}
import org.apache.commons.text.StringEscapeUtils

object ExampleLoader {

  final case class Row(qTitle: String, qTags: List[String],
                       title: String,
                       creationDate: Date, score: Int, viewCount: Int, body: String,
                       ownerDisplayName: String) {

    def isTagged(tag: String): Boolean = {
      qTitle.toLowerCase.contains(tag) || qTags.contains(tag)
    }

    def tags(allowedTags: List[String]): List[String] = {
      (allowedTags.filter(isTagged) ++ qTags.filter(allowedTags.contains)).distinct
    }

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
    val fmt = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    def apply(map: java.util.Map[String, String]): Row = {
      val vcs = map.get("ViewCount")
      val vc = if (vcs.isEmpty) 0 else vcs.toInt
      val qTags = map.get("QTags").split(",").toList.map(t ⇒ t.replace("<", "").replace(">", ""))
      Row(map.get("QTitle"), qTags,

        map.get("Title"), fmt.parse(map.get("CreationDate")),
        map.get("Score").toInt, vc, map.get("Body"),
        map.get("OwnerDisplayName")
      )
    }
  }

  def loadLabels(directory: Directory): List[Row] = {
    import scala.collection.JavaConverters._
    val schema = CsvSchema.emptySchema().withHeader()
    directory.findAll(".csv").flatMap { csvFile ⇒
      val mapper = new CsvMapper()
      val jcontents: MappingIterator[java.util.Map[String, String]] =
        mapper.readerFor(classOf[java.util.Map[_, _]]).`with`(schema).readValues(csvFile)
      val contents = jcontents.asScala.map(Row.apply).toList
      contents
    }
  }

}

object X {
  import ExampleLoader._

  def main(args: Array[String]): Unit = {
    val dir = Directory("/Users/janmachacek/Downloads/so")
    val y = loadLabels(dir).filter(_.isTagged("akka"))
    y.foreach { r ⇒
      if (r.body.contains("bad")) {
        println(s"*** bad: ${r.body}")
      }
    }
  }
}
