package com.acme

import scalaz.Monad
import scalaz.effect.IO

import scala.concurrent.Future
import scala.language.higherKinds

/**
  * The companion object contains the implicit conversions for our DSL. It is not possible
  * to define `implicit class` outside some container (like object or class or trait).
  */
object Expr {

  /**
    * The DSL methods in `RichA` and `RichExpr` would be the same, so it makes sense
    * to refactor them into a trait that can then be implemented by the two implicit
    * classes.
    *
    * The only complication that the refactoring brings is the need to define the
    * left-hand side of the expression as an abstract member; I used `def self: Expr[A]`
    * here.
    *
    * @tparam A The type we're enriching
    */
  trait ExprOps[A] {
    /**
      * The "self", or the left-hand side of the expression on which the methods
      * listed below operate; think `self.plus(5)` for example.
      *
      * @return the LHS as Expr[A]
      */
    def self: Expr[A]

    /* *
     * The following methods implement the DSL; it is necessary to allow the DSL to
     * operate on the type `A` as well as `Expr[A]`.
     * */
    def plus(right: A): Expr[A] = Plus(self, Const(right))
    def plus(right: Expr[A]): Expr[A] = Plus(self, right)
    def div(right: Expr[A]): Expr[A] = Div(self, right)
    def div(right: A): Expr[A] = Div(self, Const(right))
    def minus(right: Expr[A]): Expr[A] = Minus(self, right)
    def minus(right: A): Expr[A] = Minus(self, Const(right))
    def mult(right: A): Expr[A] = Mult(self, Const(right))
    def mult(right: Expr[A]): Expr[A] = Mult(self, right)
  }

  /**
    * Provides implicit conversion from some type `A` to add the DSL methods.
    * The value of type `A` is the constant expression; this allows, for example,
    * `5.plus ...` where `A = Int`; `5.11.minus ...` where `A = Double`, etc.
    *
    * @param const the left-hand side constant
    * @tparam A The type we're enriching
    */
  implicit class RichA[A](const: A) extends ExprOps[A] {
    override val self: Expr[A] = Const(const)
  }

  /**
    * Provides implicit conversion from `Expr[A]` to add the DSL methods. Just like
    * in `RichA`, the value of type `Expr[A]` is the left-hand side.

    * @param self the left-hand side expression
    * @tparam A The type we're enriching
    *
    * @note Note the use of `val self: Expr[A]` as the constructor parameter;
    *       using `val` yields the `self` getter, which satisfies the abstract
    *       member in `ExprOps[A]`.
    */
  implicit class RichExpr[A](val self: Expr[A]) extends ExprOps[A]

}

/**
  * The sum type for the expressions that the evaluator supports. The syntax is somewhat
  * clumsy, ML-like languages typically bring more convenient syntax, for example in Haskell:
  *
  * ``
  * data Expr a = Plus (Expr a) (Expr a) | ... | Const a
  * ``
  *
  * @tparam A the underlying expression type
  */
sealed trait Expr[+A]
case class Plus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Minus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Div[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Mult[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Const[A](const: A) extends Expr[A]

object Evaluator {

  /**
    * To be even more expressive, define alias for String
    */
  type Error = String

  /**
    * Evaluates `Expr[A]` into `Either[Error, A]`; where by convention, the value `Left` indicates
    * an error (such as division by 0), and the value `Right` indicates successfully computed result.
    *
    * This function also requires the `Fractional[A]` typeclass instance for the type `A`. The
    * typeclass defines the arithmetical operations the evaluator uses.
    *
    * @param expr the expression to be evaluated
    * @param N the typeclass instance
    * @tparam A the underlying expression type
    * @return the result of evaluating the expression
    *
    * @note the implementation uses the right-biased `flatMap` operation on the `Either`
    *       values in the recursive evaluation; this makes it convenient to use the
    *       `for` expression and the shove operator.
    */
  def eval[A](expr: Expr[A])(implicit N: Fractional[A]): Either[Error, A] = expr match {
    case Plus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.plus(l, r)
    case Minus(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.minus(l, r)
    case Mult(l, r) ⇒ for { l ← eval(l); r ← eval(r) } yield N.times(l, r)
    case Div(l, r) ⇒
      for {
        l ← eval(l)

        /* *
         * The only complication is that `filter` is not available on `Either`
         * (how could it be? if the predicate evaluates to `false`, there still
         * needs to be a value on the Left).
         * */
        r ← eval(r).filterOrElse(_ != N.zero, s"$l / $r, but $r evaluates to 0.")
      } yield N.div(l, r)

    /* *
     * The only trivial case is `Const(a)`, which evaluates to `Right(a)`
     * */
    case Const(c) ⇒ Right(c)
  }

}

// Use this import to indicate to the compiler that we _intend_ to use higher-kinded types
// This is considered a complex construct, and the compiler emits a warning without this
// import to tell us that "this is complicated; are you sure?"
import language.higherKinds

/**
  * A contravariant writer is capable of emitting the value of `A` into some underlying
  * storage our output
  *
  * @tparam A the type to be emitted
  */
trait Writer[-A] {
  /**
    * Performs the actual write operation; such operation has side-effects, so it is
    * important to return `M[Unit]` instead of just `Unit`. This not only expresses the
    * side-effecting nature of this method, but it also allows for flexible implementations.
    *
    * One such implementation might use asynchronous writes, but if the return type were
    * simply `Unit`, the implementation would have to be blocking.
    *
    * @param value the value to be written
    * @tparam M the higher-kinded type that lies in the `Monad` typeclass
    * @return the monadically-wrapped `Unit`
    */
  def write[M[_]: Monad](value: A): M[Unit]
}

/**
  * The companion object defines a simple `println` writer
  */
object Writer {

  /**
    * Returns a trivial writer; though other implementations might actually do something
    * useful like writing to a database, file, etc
    *
    * @tparam A the value to be written
    * @return the wrapped Unit
    */
  def apply[A](): Writer[A] = new Writer[A] {
    override def write[M[_]](value: A)(implicit M: Monad[M]): M[Unit] = {
      // Note the usage of M.pure; in Haskell, this would be `return`
      M.pure(println(value))
    }
  }
}

object EvaluatorMain extends App {

  import Expr._

  // The pattern-match in `val`, where we are _sure_ that the expression
  // will evaluate to `Right`. We just want the value.
  val Right(e) = Evaluator.eval[BigDecimal](Const(4))
  println(e)

  // Here, we use our DSL
  val x = Evaluator.eval((5.4 plus math.Pi) div (math.Pi minus math.Pi))
  println(x)
  val ee = Evaluator.eval(5.5 minus 8)
  println(ee)

  // Finally, we use the `Writer` implementations...
  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.std.scalaFuture.futureInstance

  // One using asynchrony and `Future`s
  Writer().write[Future](1 plus 3).foreach(_ => println("Done"))

  // One using just simple explicit I/O
  Writer().write[IO](1 plus 3).unsafePerformIO()
}
