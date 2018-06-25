package com.acme

//noinspection ScalaFileName
object FizzBuzzMain extends App {

  /**
    * Single fizz-buzz transformation; a function `Int => String`, which
    * implements the famous algorithm
    *
    * @param i the input value
    * @return the FB string
    */
  def fb(i: Int): String =
    if (i % 15 == 0) "FizzBuzz"
    else if (i % 5 == 0) "Buzz"
    else if (i % 3 == 0) "Fizz"
    else i.toString

  // Using the function is the same as Java or any other C-like language
  println(fb(1))
  println(fb(30))

  // Array(16).join("wat" - 1) + ", Batman!"
  // credit to https://www.destroyallsoftware.com/talks/wat
  println((1 to 100).map(fb).mkString(", "))
}
