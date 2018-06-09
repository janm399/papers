package com.acme

sealed trait Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Minus(left: Expr, right: Expr) extends Expr
case class Const(const: Int) extends Expr

object Evaluator {

  def eval(expr: Expr): Int = expr match {
    case Plus(l, r) ⇒ eval(l) + eval(r)
    case Minus(l, r) ⇒ eval(l) - eval(r)
    case Const(c) ⇒ c
  }

}

object EvaluatorMain extends App {

  val e = Evaluator.eval(Plus(Plus(Const(4), Const(5)), Const(3)))
  println(e)

}