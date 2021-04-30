package playground

import akka.actor.ActorSystem

object Playground extends App {

  val actorSystem = ActorSystem("HelloAkka")
  println(s"Hello, ${actorSystem.name}")

}
