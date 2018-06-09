package com.acme

object Expr {
  import language.implicitConversions
  trait ExprOps {
    def self: Expr
    def plus(right: Expr): Expr = Plus(self, right)
    def minus(right: Expr): Expr = Minus(self, right)
    def ÷(right: Expr): Expr = ???
  }

  implicit class RichInt(const: Int) extends ExprOps {
    override val self: Expr = Const(const)
  }
  implicit class RichExpr(val self: Expr) extends ExprOps
  implicit def intToExpr(const: Int): Expr = Const(const)

}

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

  import Expr._
  val ee = Evaluator.eval((5 plus 10) minus 8)
  println(ee)
}