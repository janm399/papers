package com.acme.protocolchaos

import com.acme.protocolchaos.extractors.MessageExtractor
import com.acme.protocolchaos.generators.MessageGenerator
import org.scalacheck.Gen
import org.scalacheck.rng.Seed


object Chaos {

  def main(args: Array[String]): Unit = {
    val parameters = Gen.Parameters.default.withSize(1999)
    def seed = Seed(System.currentTimeMillis())
    (0 to 100).foreach { _ ⇒
      import MessageGenerator.hints._
      MessageExtractor.messages.foreach { companion ⇒
        val message = MessageGenerator.message(companion).apply(parameters, seed)
        println(message)
      }
    }
  }

}
