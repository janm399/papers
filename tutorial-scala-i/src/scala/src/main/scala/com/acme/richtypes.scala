package com.acme

import java.time.Period
import java.util.Date

import scalaz.Monad
import scalaz.effect.IO

import scala.concurrent.Future

sealed trait Subscription
case class OneOff(endDate: Date) extends Subscription
case class Recurring(startDate: Date, period: Period) extends Subscription

trait Reader[+A] {
  def read: A
}
object Reader {
  def apply[A](): Reader[A] = new Reader[A] {
    override def read: A = ???
  }
}

import language.higherKinds

trait Writer[-A] {
  def write[M[_]: Monad](value: A): M[Unit]
}
object Writer {
  def apply[A](): Writer[A] = new Writer[A] {
    override def write[M[_]](value: A)(implicit M: Monad[M]): M[Unit] = {
      M.pure(println(value))
    }
  }
}

object RichTypes extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.std.scalaFuture.futureInstance

  Writer[Subscription]().write[Future](OneOff(new Date())).foreach(_ ⇒ println("Done"))
  Writer[Subscription]().write[IO](OneOff(new Date())).unsafePerformIO()

  Thread.sleep(1000)
}
