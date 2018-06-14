package com.acme

import java.time.Period
import java.util.Date

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

trait Writer[-A] {
  def write(value: A): Unit
}
object Writer {
  def apply[A](): Writer[A] = new Writer[A] {
    override def write(value: A): Unit = ???
  }
}

object VarianceMain extends App {
  val w1: Writer[Subscription] = Writer.apply()[Subscription]
  val w2: Writer[OneOff] = Writer.apply()[OneOff]
  val w3: Writer[Recurring] = Writer.apply()[Subscription]

}
