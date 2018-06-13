package com.acme

trait X

object ForExpressionMain extends App {

  val listOfTen = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  val x = for {
    x ← listOfTen
    y ← listOfTen
  } println(x, y)

  val y = for {
    x ← listOfTen
    y = listOfTen
  } yield (x, y)

}
