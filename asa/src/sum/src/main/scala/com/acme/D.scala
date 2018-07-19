package com.acme

import java.text.SimpleDateFormat

object D {
  def main(args: Array[String]): Unit = {
    val r = ExampleLoader.loadLabels(Directory("/Users/janmachacek/Downloads/sox")).minBy(_.creationDate)
    val fmt = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    println(fmt.format(r.creationDate))
  }
}
