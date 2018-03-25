package com.acme.protocolchaos.user

import akka.actor.{Actor, ActorLogging, Props}
import cakesolutions.kafka.akka.{ConsumerRecords, KafkaConsumerActor}
import cakesolutions.kafka.{KafkaConsumer, KafkaDeserializer}
import com.acme.protocolchaos.Register
import com.typesafe.config.Config
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.Future
import scala.util.Try

object UsersActor {
  def props(config: Config): Props = {
    val conf = KafkaConsumer.Conf(
      config.getConfig("app.kafka.consumer-config"),
      new StringDeserializer(),
      KafkaDeserializer(Register.validate)
    )
    Props(new UsersActor(conf))
  }
}

class UsersActor(consumerConf: KafkaConsumer.Conf[String, Try[Register]])
  extends Actor with ActorLogging {
  private val extractor = ConsumerRecords.extractor[String, Try[Register]]
  private val userRegistrator = new User.Registrar(context.system)

  override def preStart(): Unit = {
    KafkaConsumerActor(consumerConf, KafkaConsumerActor.Conf(), self)
      .subscribe(KafkaConsumerActor.Subscribe.AutoPartition(Seq("user-registration-v1")))
  }

  override def receive: Receive = {
    case extractor(batch) ⇒
      log.info("Attempting to register {} users", batch.values.length)
      val sndr = sender()
      import context.dispatcher
      val parsedUsers = batch.values.flatMap(_.toOption)
      log.info("Attempting to register {} valid users", parsedUsers.length)
      val validUsers = parsedUsers.filter(User.Registrar.isValid)
      log.info("Will register {} users", validUsers.length)
      Future.sequence(validUsers.map(userRegistrator.register)).foreach { registeredUsers ⇒
        sndr ! KafkaConsumerActor.Confirm(batch.offsets, commit = true)
        log.info("Registered {} users", registeredUsers.length)
      }
  }
}
