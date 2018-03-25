package com.acme.protocolchaos.user

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorLogging, Kill, OneForOneStrategy, Props, SupervisorStrategy}
import cakesolutions.kafka.akka.{ConsumerRecords, KafkaConsumerActor}
import cakesolutions.kafka.{KafkaConsumer, KafkaDeserializer}
import com.acme.protocolchaos.Register
import com.typesafe.config.Config
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.Future

object UsersActor {
  def props(config: Config): Props = {
    val conf = KafkaConsumer.Conf(
      config.getConfig("app.kafka.consumer-config"),
      new StringDeserializer(),
      KafkaDeserializer(Register.parseFrom)
    )
    Props(new UsersActor(conf))
  }
}

class UsersActor(consumerConf: KafkaConsumer.Conf[String, Register])
  extends Actor with ActorLogging {
  private val extractor = ConsumerRecords.extractor[String, Register]
  private val userRegistrator = new User.Registrar(context.system)

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 3) {
    case _ ⇒ Restart
  }

  override def preStart(): Unit = {
    KafkaConsumerActor(consumerConf, KafkaConsumerActor.Conf(), self)
      .subscribe(KafkaConsumerActor.Subscribe.AutoPartition(Seq("user-registration-v1")))
  }

  override def receive: Receive = {
    case extractor(batch) ⇒
      log.info(s"Will register ${batch.values}")
      val sndr = sender()
      import context.dispatcher
      Future.sequence(batch.values.map(userRegistrator.register)).foreach { _ ⇒
        sndr ! KafkaConsumerActor.Confirm(batch.offsets, commit = true)
        log.info(s"Registered ${batch.values}")
      }
    case KafkaConsumerActor.ConsumerException(_, _, ex) ⇒
      log.error(ex, "Consumer exception")
      self ! Kill
  }
}
