package com.acme.protocolchaos.user

import java.text.SimpleDateFormat
import java.util.Date

import akka.actor.ActorSystem
import akka.http.impl.model.parser.UriParser
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{DateTime, HttpMethods, HttpRequest, Uri}
import akka.stream.ActorMaterializer
import com.acme.protocolchaos.{ProfileImage, Register}
import com.google.protobuf.ByteString

import scala.concurrent.Future
import scala.util.Try

case class User(id: String, dob: Date, tags: Seq[String], profileImage: Option[ProfileImage])

object User {

  object Registrar {

    def isValid(register: Register): Boolean = {
      val lu = register.getUser
      Try(new SimpleDateFormat("dd-MM-yyyy").parse(lu.dob)).isSuccess &&
      Try(register.profileImageSource match {
        case Register.ProfileImageSource.ProfileImageUrl(url) ⇒ Uri(url)
        case _ ⇒
      }).isSuccess
    }

  }

  class Registrar(system: ActorSystem) {
    private implicit val materializer: ActorMaterializer = ActorMaterializer()(system)
    import system.dispatcher

    def register(register: Register): Future[User] = {
      val lu = register.getUser
      val dob = new SimpleDateFormat("dd-MM-yyyy").parse(lu.dob)
      val u = User(lu.id, dob, lu.tags, None)

      import scala.concurrent.duration._

      val pif = register.profileImageSource match {
        case Register.ProfileImageSource.ProfileImageUrl(url) ⇒
          Http(system)
            .singleRequest(HttpRequest(method = HttpMethods.GET, uri = Uri(url)))
            .flatMap(_.entity.toStrict(3.seconds))
            .map(e ⇒ Some(ProfileImage(e.contentType.value, ByteString.copyFrom(e.data.toByteBuffer))))
            .recover { case _ ⇒ None }
        case Register.ProfileImageSource.ProfileImage(profileImage) ⇒
          Future.successful(Some(profileImage))
        case Register.ProfileImageSource.Empty ⇒
          Future.successful(None)
      }
      pif.map(pi ⇒ u.copy(profileImage = pi))
    }
  }

}
