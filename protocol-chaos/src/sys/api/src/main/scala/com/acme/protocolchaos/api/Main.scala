package com.acme.protocolchaos.api

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import cakesolutions.kafka.{KafkaProducer, KafkaSerializer}
import com.trueaccord.scalapb.GeneratedMessage
import com.typesafe.config.ConfigFactory
import org.apache.kafka.common.serialization.StringSerializer

object Main extends App {
  private val config = ConfigFactory.load("application.conf")
  implicit private val system: ActorSystem = ActorSystem(name = "api", config = config)
  implicit private val materializer: ActorMaterializer = ActorMaterializer()
  private val producer = KafkaProducer(
    KafkaProducer.Conf(config.getConfig("app.kafka.producer-config"),
    new StringSerializer(),
    KafkaSerializer[GeneratedMessage](_.toByteArray)
  ))
  private val service = new ApiService(producer)(system.dispatcher)

  Http(system).bindAndHandle(service.route, "0.0.0.0", port = 8080)
}
