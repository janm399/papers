package com.acme

//noinspection ScalaFileName
object FizzBuzzMain extends App {
  def fb(i: Int): String = "All work and no play makes Jack a dull boy"

  println(fb(1))
  println(fb(30))

  // Array(16).join("wat" - 1) + ", Batman!"
  // credit to https://www.destroyallsoftware.com/talks/wat
  println((1 to 100).map(fb).mkString(", "))
}
