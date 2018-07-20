package com.acme

import java.io.File

trait Directory {
  def findAll(extensions: String*): List[File]
}

object Directory {

  def apply(spath: String, excludes: List[String] = List("target")): Directory = {
    val path = spath.replace("~", System.getProperty("user.home"))

    new Directory {

      private def find(extensions: Seq[String])(dir: File): Array[File] = {
        if (dir.isDirectory && !excludes.contains(dir.getName)) {
          val files = dir.listFiles()
          files.filter(f ⇒ f.isFile && extensions.exists(ext ⇒ f.getName.endsWith(ext))) ++
            files.filter(_.isDirectory).flatMap(find(extensions))
        } else Array.empty
      }

      override def findAll(extensions: String*): List[File] = {
        find(extensions)(new File(path)).toList
      }

    }
  }

}
