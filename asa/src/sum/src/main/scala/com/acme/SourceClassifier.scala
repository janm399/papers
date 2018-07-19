package com.acme

import java.io.File

trait SourceClassifier {

  def classify(source: File): Unit

}

object SourceClassifier {
  private val stopwords = Set("<-", "←", "<:", "<%", "=", "=>", "⇒", ">:", "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")

  def apply(): SourceClassifier = new SourceClassifier {

    override def classify(source: File): Unit = ???

  }

}
