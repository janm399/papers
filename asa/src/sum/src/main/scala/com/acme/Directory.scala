package com.acme

import java.io.File

trait Directory {
  def exclude(patterns: String*): Directory
  def fileName: String

  def findFilesR(extensions: String*): List[File]
  def findDirectories(): List[Directory]

  def relativeFileName(file: File): String
}

object Directory {

  def apply(spath: String): Directory = {
    val path = spath.replace("~", System.getProperty("user.home"))
    new FileDirectory(new File(path), Nil)
  }

  private class FileDirectory(path: File, excludePatterns: List[String]) extends Directory {

    override def fileName: String = path.getName

    override def findDirectories(): List[Directory] = {
      path.listFiles().toList.filter(_.isDirectory).map(f ⇒ new FileDirectory(f, excludePatterns))
    }

    override def exclude(patterns: String*): Directory = new FileDirectory(path, patterns.toList)

    override def relativeFileName(file: File): String = file.getAbsolutePath.replace(path.getAbsolutePath, ".")

    override def findFilesR(extensions: String*): List[File] = {
      def find(extensions: Seq[String])(dir: File): List[File] = {
        if (dir.isDirectory && !excludePatterns.contains(dir.getName)) {
          val files = dir.listFiles().toList
          files.filter(f ⇒ f.isFile && extensions.exists(ext ⇒ f.getName.endsWith(ext))) ++
            files.filter(_.isDirectory).flatMap(find(extensions))
        } else List.empty
      }

      find(extensions)(path)
    }
  }

}
