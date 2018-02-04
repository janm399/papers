package com.acme.protocolchaos

import scala.util.Random

trait Utils {

  final def escape(data: Array[Byte]): String = {
    val cbuf = new StringBuilder
    for (b <- data) {
      if (b >= 0x20 && b <= 0x7e) cbuf.append(b.toChar)
      else cbuf.append("\\%02X".format(b & 0xff))
    }
    cbuf.toString
  }

  final def alphaString(length: Int): String = Random.alphanumeric.take(length).mkString("")

  final def time[A](f: ⇒ A): (A, Long) = {
    val s = System.currentTimeMillis()
    val r = f
    (r, System.currentTimeMillis() - s)
  }

  final def bitString(b: Byte): String = (0 until 8).map(bit ⇒ if ((b & bit) != 0) "1" else "0").mkString("")

  implicit class RichByte(b: Byte) {

    def bitString: String = (0 until 8).map(bit ⇒ if ((b & bit) != 0) "1" else "0").mkString("")

    def bitDifference(other: Byte): Int = {
      (0 until 8).foldLeft(0) { (res, bit) ⇒
        val b1 = (b & bit) != 0
        val b2 = (other & bit) != 0
        if (b1 == b2) res else res + 1
      }
    }

  }

}

object Utils extends Utils