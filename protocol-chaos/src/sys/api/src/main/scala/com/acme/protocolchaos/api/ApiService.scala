package com.acme.protocolchaos.api

import akka.http.scaladsl.server.{Directives, Route}
import cakesolutions.kafka.{KafkaProducer, KafkaProducerRecord}
import com.acme.protocolchaos.Register
import com.trueaccord.scalapb.GeneratedMessage

import scala.concurrent.ExecutionContext

class ApiService(producer: KafkaProducer[String, GeneratedMessage])(implicit executor: ExecutionContext)
  extends Directives with ProtobufMarshallers with Json4sMarshallers {

  def route: Route = {
    path("user") {
      post {
        handleWith { register: Register ⇒
          producer.
            send(KafkaProducerRecord("user-registration-v1", register.getUser.id, register)).
            map(_ ⇒ "OK")
        }
      }
    }
  }

}
