package com.acme.protocolchaos.chaos.generators

import org.scalacheck.Gen

object UriGenerators {

  private val schemeGen = Gen.oneOf("https", "https", "ws", "wss")

  val uri: Gen[String] = for {
    scheme ← schemeGen
    segmentCount ← Gen.chooseNum(1, 10)
    segments ← Gen.listOfN(segmentCount, Gen.alphaNumStr)
  } yield s"$scheme://${segments.mkString("/")}"

}
