package com.acme

import scala.language.higherKinds
import scala.reflect.ClassTag

object Expr {

  trait ExprOps {
    def self: Expr
    def plus[A : ClassTag](right: A): Expr = Plus(self, Const(right))
    def plus(right: Expr): Expr = Plus(self, right)
    def div(right: Expr): Expr = Div(self, right)
    def div[A : ClassTag](right: A): Expr = Div(self, Const(right))
    def minus(right: Expr): Expr = Minus(self, right)
    def minus[A : ClassTag](right: A): Expr = Minus(self, Const(right))
  }

  implicit class RichA[A : ClassTag](const: A) extends ExprOps {
    override val self: Expr = Const(const)
  }
  implicit class RichExpr(val self: Expr) extends ExprOps

}

sealed trait Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Minus(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Const[A : ClassTag](const: A) extends Expr

trait Applicative[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](f: A ⇒ B): F[B]
}

object Evaluator {

  def eval[A : ClassTag, F[_]](expr: Expr)(implicit N: Fractional[A], F: Applicative[F]): F[A] = expr match {
    case Plus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.plus(l, r)
    case Minus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.minus(l, r)
    case Div(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.div(l, r)
    case Const(c: A) ⇒ F.pure(c)
  }

}

object EvaluatorMain extends App {

  val e = Evaluator.eval[BigDecimal, Option](Plus(Plus(Const(4), Const(5)), Const(3)))
  println(e)

  import Expr._
  val ee = Evaluator.eval[Double, Option]((5.4 plus math.Pi) minus 8.8)
  println(ee)
}
