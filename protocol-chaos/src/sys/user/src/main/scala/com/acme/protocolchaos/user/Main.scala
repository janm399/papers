package com.acme.protocolchaos.user

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

object Main extends App {
  private val config = ConfigFactory.load("application.conf")
  private val system = ActorSystem(name = "user", config = config)

  //noinspection ScalaUnusedSymbol
  private val users = system.actorOf(UsersActor.props(config))
}