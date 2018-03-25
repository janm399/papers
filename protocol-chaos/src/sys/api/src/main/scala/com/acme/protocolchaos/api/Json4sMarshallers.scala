package com.acme.protocolchaos.api

import akka.http.scaladsl.marshalling.GenericMarshallers
import akka.http.scaladsl.model.{ContentTypeRange, ContentTypes}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString
import org.json4s.{DefaultFormats, Formats, Reader, jackson}
import org.json4s.jackson.{JsonMethods, Serialization}

/**
  * Provides support for Json4s marshalling; to use, implement `Reader` or `JsonFormat` for the
  * types you wish to have marshallers and unmarshallers derived.
  */
trait Json4sMarshallers extends GenericMarshallers {

  implicit val serialization: Serialization.type = jackson.Serialization // or native.Serialization
  implicit val formats: Formats = DefaultFormats

  val unmarshallerContentTypes: Seq[ContentTypeRange] =
    List(ContentTypes.`application/json`)

  private val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(unmarshallerContentTypes: _*)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset) => data.decodeString(charset.nioCharset.name)
      }

  implicit def unmarshaller[A : Reader]: FromEntityUnmarshaller[A] =
    jsonStringUnmarshaller.map(x ⇒ JsonMethods.parse(x)).map(Formats.read[A])

}
