package com.acme.protocolchaos.generators

import org.scalacheck.Gen

object IdGenerator {

  val id: Gen[String] = Gen.oneOf(Gen.uuid.map(_.toString), Gen.alphaNumStr)

}
