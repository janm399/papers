package com.acme.protocolchaos.chaos

import java.util

import org.apache.kafka.common.serialization.Serializer

import scala.util.Random

object EvilSerializer {

  implicit class ES[A](inner: Serializer[A]) {
    def evil: Serializer[A] = new EvilSerializer[A](inner)
  }

  def apply[A](inner: Serializer[A]): Serializer[A] =
    new EvilSerializer[A](inner)

}

class EvilSerializer[T](inner: Serializer[T]) extends Serializer[T] {

  override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = inner.configure(configs, isKey)

  private val FLIP_BIT_DIVISOR = 30
  private val TRIM_DIVISOR = 10
  private val FLIP_DIVISOR = 10
  private val SO_DIVISOR = 10

  override def serialize(topic: String, data: T): Array[Byte] = {

    def flipBit(x: Byte): Byte = {
      if (Random.nextInt() % FLIP_BIT_DIVISOR == 0) (x & 0xf7).toByte else x
    }

    val serialized = inner.serialize(topic, data)
    if (Random.nextInt() % TRIM_DIVISOR == 0) {
      Array.emptyByteArray
    } else if (Random.nextInt() % FLIP_DIVISOR == 0) {
      serialized.map(flipBit)
    } else if (Random.nextInt() % SO_DIVISOR == 0) {
      val bp = Array.fill(serialized.length + 20000)(99.toByte)
      Array.copy(serialized, 0, bp, 0, serialized.length)
      bp
    } else serialized
  }

  override def close(): Unit = inner.close()

}
