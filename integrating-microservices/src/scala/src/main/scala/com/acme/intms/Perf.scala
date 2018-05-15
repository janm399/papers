package com.acme.intms

object Perf {

  def fb1(i: Int): String =
    if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
    else if (i % 3 == 0) "Fizz"
    else if (i % 5 == 0) "Buzz"
    else i.toString

  def fb2a(i: Int): String = {
    def fb = {
      if (i % 3 == 0 && i % 5 == 0) throw new RuntimeException("FizzBuzz")
      else if (i % 3 == 0) "Fizz"
      else if (i % 5 == 0) "Buzz"
      else i.toString
    }

    try {
      fb
    } catch {
      case re: RuntimeException ⇒ re.getMessage
    }
  }

  def fb2b(i: Int): String = {
    def fb = {
      if (i % 3 == 0 && i % 5 == 0) throw new RuntimeException("FizzBuzz")
      else if (i % 3 == 0) throw new Exception("Fizz")
      else if (i % 5 == 0) throw new IllegalArgumentException("Buzz")
      else throw new ClassCastException(i.toString)
    }

    try {
      fb
    } catch {
      case e: Exception ⇒ e.getMessage
    }
  }

  def fb3(i: Int): String = {
    trait ConditionChecker[In] {
      def check(in: In): Boolean
    }
    trait Const[C] {
      def const: C
    }
    val Fizz = new Const[String] {
      override def const: String = "Fizz"
    }
    val Buzz = new Const[String] {
      override def const: String = "Buzz"
    }
    val FizzBuzz = new Const[String] {
      override def const: String = "FizzBuzz"
    }
    val fizz = new ConditionChecker[Int] {
      override def check(in: Int): Boolean = in % 3 == 0
    }
    val buzz = new ConditionChecker[Int] {
      override def check(in: Int): Boolean = in % 5 == 0
    }
    val fizzBuzz = new ConditionChecker[Int] {
      override def check(in: Int): Boolean = fizz.check(in) && buzz.check(in)
    }
    val other = new ConditionChecker[Int] {
      override def check(in: Int): Boolean = !(fizz.check(in) || buzz.check(in) || fizzBuzz.check(in))
    }

    if (fizzBuzz.check(i)) FizzBuzz.const
    else if (fizz.check(i)) Fizz.const
    else if (buzz.check(i)) Buzz.const
    else if (other.check(i)) i.toString
    else throw new RuntimeException("Absurd!")
  }

  def naive(count: Int, f: Int ⇒ String): String = {
    var result = f(1)
    for (i ← 2 until count) {
      result += ", " + f(i)
    }
    result
  }

  def smart(count: Int, f: Int ⇒ String): String = {
    val result = new StringBuilder(f(1))
    var i = 2
    while (i < count) {
      result.append(", ").append(f(i))
      i += 1
    }
    result.toString()
  }

  def timed[A](f: ⇒ A): (Long, A) = {
    val start = System.currentTimeMillis()
    var i = 0
    while (i < 100000) {
      f
      i += 1
    }
    val elapsed = System.currentTimeMillis() - start
    (elapsed, f)
  }

  def main(args: Array[String]): Unit = {
    def run = {
      println(timed(naive(100, fb1)))
      println(timed(naive(100, fb2a)))
      println(timed(naive(100, fb2b)))
      println(timed(naive(100, fb3)))

      println(timed(smart(100, fb1)))
      println(timed(smart(100, fb2a)))
      println(timed(smart(100, fb2b)))
      println(timed(smart(100, fb3)))
    }

    for (_ ← 0 to 100) run
  }

}
