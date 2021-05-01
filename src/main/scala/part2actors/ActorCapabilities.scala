package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.BankAccount.{ Deposit, TransactionFailure, TransactionSuccess, Withdraw}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi" =>
        println(s"[${self}] Someone said Hi to me")
        context.sender() ! "Hello, there"
      case message: String => println(s"[${self}] I have received a message: $message")
      case number: Int => println(s"[simple actor] I have a received a number $number")
      case SpecialMessage(content) => println(s"[simple actor] I have received something spacial: $content")
      case NoteToSelf(note) => self ! note
      case SayHiTo(ref) => ref ! "Hi"
      case GameOfTelephone(contant, ref) => ref forward contant + "s"
    }
  }

  val actorSystem = ActorSystem("SimpleActorDemo")
  val simpleActor = actorSystem.actorOf(Props[SimpleActor], "simple-actor")

  simpleActor ! "Super important message"

  // 1- messages can be of any type
  //    -> messages must be IMMUTABLE
  //    -> messages must be serializable
  // ==> In practice, use case classes
  simpleActor ! 13

  case class SpecialMessage(content: String)
  simpleActor ! SpecialMessage("Some special content")

  // 2- Actors have information about theur context and themselves
  // context.self === self <==> 'this' in OOP.

  // Actors can use self to send messages to themselves
  case class NoteToSelf(note: String)
  simpleActor ! NoteToSelf("I'm a very special actor")

  // 3- Actors can reply to message
  val alice = actorSystem.actorOf(Props[SimpleActor], "alice")
  val bob = actorSystem.actorOf(Props[SimpleActor], "bod")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // (<-- The line above reads 'alice tell SayHiTo bob'
  // We can improve it with a more natural dsl
  object SayHiHelpers{
    implicit class HiExtensions(sender: ActorRef) {
      def sayHiTo(ref: ActorRef) = sender ! SayHiTo(ref)
    }
  }
  import SayHiHelpers._
  bob sayHiTo alice //  reads more naturally -->)

  // 4- dead letters
  // If we send the message "Hi" to alice from here,
  // alice will send "Hello, there" back to the sender
  // but who is the sender
  //alice ! "Hi"  // --> dead letters encountered. when alice does `context.sender ! "Hello, there"`

  // 5- forwarding
  // Sending with original sender as reference
  case class GameOfTelephone(contant: String, ref: ActorRef)
  alice ! GameOfTelephone("Hi", bob)

  // Exercise
  // 1- Create a counter actor
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }
  import Counter._
  class Counter extends Actor {

    var number: Int = 0
    override def receive: Receive = {
      case Increment => number += 1
      case Decrement => number -= 1
      case Print => println(s"Current count is $number")
    }
  }

  val counter1 = actorSystem.actorOf(Props[Counter], "counter-actor")
  (1 to 10).foreach(_ => counter1 ! Increment)
  counter1 ! Print
  counter1 ! Decrement
  counter1 ! Print

  // 2- Account actor
  object BankAccount {
    case class Deposit(amount: Double)
    case class Withdraw(amount: Double)
    case object Statement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(reason: String)
  }
  class BankAccount extends Actor {
    import BankAccount._
    var balance = 0.0
    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount < 0)
          sender() ! TransactionFailure("Invalid deposit amount")
        else {
          balance += amount
          sender() ! TransactionSuccess(s"Successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if (amount < 0)
          sender() ! TransactionFailure("Invalid withdraw amount")
        else if (amount > balance) {
          sender() ! TransactionFailure("Insufficient balance")
        } else {
          balance -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }
      case Statement => sender() ! s"Current balance is $balance"

    }


  }

  object BankCustomer {
    case class LiveTheLife(account: ActorRef)
  }
  class BankCustomer extends Actor {
    import BankCustomer._
    import BankAccount._
    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(1000000)
        account ! Withdraw(1000)
        account ! Statement
      case message => println(message.toString)
    }
  }
  import BankCustomer._
  val account = actorSystem.actorOf(Props[BankAccount], "MyBankAccount")
  val rickyMartin = actorSystem.actorOf(Props[BankCustomer], "ricky")
  rickyMartin ! LiveTheLife(account)



}
