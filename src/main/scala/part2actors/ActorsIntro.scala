package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  // part1 - actor system
  val actorSystem = ActorSystem("first-actor-system")
  println(s"Actor system created: name ${actorSystem.name}")

  // part2 - create actor
  // word count actor
  class WordCountActor extends Actor {
    // internal data
    var totalWords = 0

    // bwhevior
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: ${message}")
        totalWords += message.split(" ").length
        //println(s" --> totalWords is now $totalWords")
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // part3 - instantiate an actor
  val wordCountActor = actorSystem.actorOf(Props[WordCountActor], "wordCounter")

  // part4 - communicating (by sending message to actorRed)
  wordCountActor ! "I am learning Akka and it is pretty fun"
  wordCountActor ! "Is this cool or is this cool"
  // -> completely asynchronous.

  val anotherWordCountActor = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")
  anotherWordCountActor ! "Another message"


  // How do you instanciate and Actor with a constructor argument
  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hello" => println(s"[Person($name)] Hi, my name is $name")
      case _ => println(s"[Person($name)] Unknown message. Just say 'hello' to me.")
    }
  }

  // -> option 1: with Props, legal  (but discouraged)
  val personActor = actorSystem.actorOf(Props(new Person("Bob")))
  personActor ! "hello"

  // -> option 2: with a props method inside companion object (best pactice)
  object Person {
    def props(name: String) = Props(new Person(name))
  }
  val anotherPersonActor = actorSystem.actorOf(Person.props("John"))
  anotherPersonActor ! "Now, this works"

  //actorSystem.terminate()
}
