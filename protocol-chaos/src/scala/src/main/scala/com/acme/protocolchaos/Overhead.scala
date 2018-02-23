package com.acme.protocolchaos

import java.security.MessageDigest
import java.util.zip.{CRC32, Checksum}

import com.trueaccord.scalapb.GeneratedMessage
import Utils._

object Overhead {

  trait RU {
    def update(b: Array[Byte]): Unit

    def reset(): Unit
  }

  class Run(m: List[GeneratedMessage], ru: RU) {
    def run(count: Int): List[(Int, Long)] =
      m.map { gm ⇒
        val b = gm.toByteArray
        time {
          var x: Int = 0
          while (x < count) {
            x += 1
            ru.reset()
            ru.update(b)
          }
          b.length
        }
      }
  }

  final class MDRU(x: MessageDigest) extends RU {
    override def update(b: Array[Byte]): Unit = x.update(b)
    override def reset(): Unit = x.reset()
  }

  final class CSRU(x: Checksum) extends RU {
    override def update(b: Array[Byte]): Unit = x.update(b, 0, b.length)
    override def reset(): Unit = x.reset()
  }

  def main(args: Array[String]): Unit = {
    import Utils._
    val md = MessageDigest.getInstance("SHA-256")
    val crc = new CRC32
    val xs = List(
      X(count = 426546456, greeting = alphaString(92)),
      X(count = 426546456, greeting = alphaString(1015)),
      X(count = 426546456, greeting = alphaString(10231)),
      X(count = 426546456, greeting = alphaString(1048566)),
      X(count = 426546456, greeting = alphaString(10485749))
    )

//    println(new Run(xs, new CSRU(new CRC32)).run(100000))
//    println(new Run(xs, new MDRU(MessageDigest.getInstance("MD5"))).run(2000))
    println(new Run(xs, new MDRU(MessageDigest.getInstance("SHA-256"))).run(2000))
  }

}
