package com.acme

import java.io.{File, FilenameFilter}

class Directory(path: String) {

  private def find(filter: FilenameFilter)(dir: File): Array[File] = {
    if (dir.isDirectory) {
      val files = dir.listFiles(filter)
      files ++ files.filter(_.isDirectory).flatMap(find(filter))
    } else Array.empty
  }

  def findAll(extensions: String*): Array[File] = {
    find((_, name) ⇒ extensions.exists(ext ⇒ name.endsWith(ext)))(new File(path))
  }

}
