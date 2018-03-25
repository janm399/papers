package com.acme.protocolchaos.api

import akka.http.scaladsl.marshalling.{GenericMarshallers, Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import com.trueaccord.scalapb._
import com.trueaccord.scalapb.json.JsonFormat
import org.json4s.JsonAST.JArray
import org.json4s.jackson.JsonMethods

/**
  * This trait provides convenient marshallers for Protocol Buffers-derived values.
  */
trait ProtobufMarshallers extends GenericMarshallers {

  /**
    * Unmarshaller from `application/json` to a protobuf-specified message
    * @tparam U the message type
    * @return the unmarshaller
    */
  implicit def protobufDerivedUnmarshaller[U <: GeneratedMessage with Message[U]: GeneratedMessageCompanion]: FromEntityUnmarshaller[U] = {
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(MediaTypes.`application/json`)
      .map(JsonFormat.fromJsonString[U])
  }

  /**
    * Unmarshaller from `application/json` to a protobuf-specified message
    * @tparam U the message type
    * @return the unmarshaller
    */
  implicit def iterableProtobufDerivedUnmarshaller[U <: GeneratedMessage with Message[U]: GeneratedMessageCompanion]: FromEntityUnmarshaller[Iterable[U]] = {
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(MediaTypes.`application/json`)
      .map(s ⇒ JsonMethods.parse(s))
      .map {
        case JArray(elements) ⇒ elements.map(JsonFormat.fromJson[U])
        case _ ⇒ Nil
      }
  }

  /**
    * Marshaller from protobuf-specified message to `application/json`
    * @tparam U the message type
    * @return the marshaller
    */
  implicit def protobufDerivedMarshaller[U <: GeneratedMessage]: ToEntityMarshaller[U] = {

    def jsonMarshaller(): ToEntityMarshaller[U] = {
      val contentType = ContentTypes.`application/json`
      Marshaller.withFixedContentType(contentType) { value ⇒
        HttpEntity(contentType, JsonFormat.toJsonString(value))
      }
    }

    Marshaller.oneOf(jsonMarshaller())
  }

  /**
    * Special case of JSON marshaller for an sequence of protobuf-specified messages
    * @tparam U the element of the sequence
    * @return the marshaller of sequence of Us
    */
  implicit def iterableMarshaller[U <: GeneratedMessage]: ToEntityMarshaller[Iterable[U]] = {

    def jsonMarshaller(): ToEntityMarshaller[Iterable[U]] = {
      val contentType = ContentTypes.`application/json`

      Marshaller.withFixedContentType(contentType) { value ⇒
        val b = StringBuilder.newBuilder
        b.append("[")
        value.foreach { x ⇒
          if (b.size > 1) b.append(",")
          b.append(JsonFormat.toJsonString(x))
        }
        b.append("]")
        // TODO: This wants to use source for large responses
        // Source.fromIterator(() ⇒ value.iterator).map(JsonFormat.toJsonString).

        HttpEntity(contentType, b.toString())
      }
    }

    Marshaller.oneOf(jsonMarshaller())
  }

}

/**
  * Singleton-scoped Protocol Buffers marshallers
  */
object ProtobufMarshallers extends ProtobufMarshallers
