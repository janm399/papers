package com.acme.protocolchaos.chaos

import cakesolutions.kafka.{KafkaProducer, KafkaProducerRecord, KafkaSerializer}
import com.acme.protocolchaos.Register
import com.acme.protocolchaos.chaos.generators.MessageGenerator
import com.trueaccord.scalapb.GeneratedMessage
import com.typesafe.config.ConfigFactory
import org.apache.kafka.common.serialization.StringSerializer
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

object Main extends App {
  import EvilSerializer._
  private val config = ConfigFactory.load("application.conf")
  private val producer = KafkaProducer(
    KafkaProducer.Conf(
      config.getConfig("app.kafka.producer-config"),
      new StringSerializer(),
      KafkaSerializer[GeneratedMessage](_.toByteArray).evil
    )
  )

  val parameters = Gen.Parameters.default.withSize(1999)
  def seed = Seed(System.currentTimeMillis())
  while (true) {
    import MessageGenerator.hints.default
    MessageGenerator.message[Register](Register).apply(parameters, seed).foreach { register ⇒
      val key = register.getUser.id
      producer.send(KafkaProducerRecord("user-registration-v1", key, register))
      Thread.sleep(100)
    }
  }

}
