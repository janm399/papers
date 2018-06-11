package com.acme

import scala.language.higherKinds
import scala.reflect.ClassTag

object Expr {

  trait ExprOps[A] {
    def self: Expr[A]
    def plus(right: A)(implicit ev: ClassTag[A]): Expr[A] = Plus(self, Const(right))
    def plus(right: Expr[A]): Expr[A] = Plus(self, right)
    def div(right: Expr[A]): Expr[A] = Div(self, right)
    def div(right: A)(implicit ev: ClassTag[A]): Expr[A] = Div(self, Const(right))
    def minus(right: Expr[A]): Expr[A] = Minus(self, right)
    def minus(right: A)(implicit ev: ClassTag[A]): Expr[A] = Minus(self, Const(right))
  }

  implicit class RichA[A : ClassTag](const: A) extends ExprOps[A] {
    override val self: Expr[A] = Const(const)
  }
  implicit class RichExpr[A : ClassTag](val self: Expr[A]) extends ExprOps[A]

}

sealed trait Expr[+A]
case class Plus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Minus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Div[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Const[A : ClassTag](const: A) extends Expr[A]

object Evaluator {

  def eval[A : ClassTag](expr: Expr[A])(implicit N: Fractional[A]): A = expr match {
    case Plus(l, r) ⇒ N.plus(eval(l), eval(r))
    case Minus(l, r) ⇒ N.minus(eval(l), eval(r))
    case Div(l, r) ⇒ N.div(eval(l), eval(r))
    case Const(c: A) ⇒ c
  }

}

object EvaluatorMain extends App {
  implicit class RichDouble(d: Double) {
    def ^(y: Double): Double = math.pow(x, y)
  }

  val x: Double = 42
  x ^ 2

  import Expr._

  val e: BigDecimal = Evaluator.eval(Const(4))
  println(e)

  val ee = Evaluator.eval((5.4 plus math.Pi) minus 8.8)
  println(ee)
}
