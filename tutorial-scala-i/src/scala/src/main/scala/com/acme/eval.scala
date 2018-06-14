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

  type Error = String

  def eval[A : ClassTag](expr: Expr[A])(implicit N: Fractional[A]): Either[Error, A] = expr match {
    case Plus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.plus(l, r)
    case Minus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.minus(l, r)
    case Div(l, r) ⇒
      for {
        l ← eval(l)
        r ← eval(r).filterOrElse(_ != N.zero, s"$l / $r, but $r evaluates to 0.")
      } yield N.div(l, r)
    case Const(c: A) ⇒ Right(c)
  }

}

case class FC[A](a: A) {
  def map[B](f: A ⇒ B): FC[B] = FC(f(a))
  def flatMap[B](f: A ⇒ FC[B]): FC[B] = f(a)
}


object EvaluatorMain extends App {

  FC("b").flatMap(x ⇒ FC(x))

  import Expr._

  val Right(e) = Evaluator.eval[BigDecimal](Const(4))
  println(e)

  val x = Evaluator.eval((5.4 plus math.Pi) div (math.Pi minus math.Pi))
  println(x)

  val ee = Evaluator.eval((5.4 plus math.Pi) minus 8.8)
  println(ee)
}
