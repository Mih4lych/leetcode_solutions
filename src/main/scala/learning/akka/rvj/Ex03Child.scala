package learning.akka.rvj

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors

import scala.collection.mutable

object Ex03Child extends App {
  object Parent {
    trait Command
    case class CreateChild(name: String) extends Command
    case class TellChild(name: String, msg: String) extends Command
    case class StopChild(name: String) extends Command
    case class WatchChild(name: String) extends Command

    def apply(): Behavior[Command] =  process(Map.empty)

    def process(children: Map[String, ActorRef[String]]): Behavior[Command] = Behaviors.receive[Command] { (context, command) =>
      command match {
        case CreateChild(name) =>
          children.get(name) match {
            case Some(_) =>
              context.log.info(s"Child $name already exists")
              Behaviors.same
            case None =>
              context.log.info(s"Child $name is creating")
              process(children.updated(name, context.spawn(Child(), name)))
          }
        case TellChild(name, msg) =>
          children.get(name) match {
            case Some(ref) =>
              ref ! msg
            case None =>
              context.log.info(s"Child $name doesn't exist")
          }
          Behaviors.same
        case StopChild(name) =>
          children.get(name) match {
            case Some(ref) =>
              context.stop(ref)
              process(children - name)
            case None =>
              context.log.info(s"Child $name doesn't exist")
              Behaviors.same
          }
        case WatchChild(name) =>
          children.get(name) match {
            case Some(ref) =>
              context.watch(ref)
            case None =>
              context.log.info(s"Child $name doesn't exist")
          }
          Behaviors.same
      }
    }
      .receiveSignal {
        case (context, Terminated(refChild)) =>
          context.log.info(s"Child ${refChild.path} is dead")
          Behaviors.same
      }
  }

  object Child {
    def apply(): Behavior[String] = Behaviors.receive { (context, msg) =>
      context.log.info(msg)
      Behaviors.same
    }
  }

  def demo(): Unit = {
    import Parent._

    val userGuardBehavior: Behavior[Unit] = Behaviors.setup { context =>
      val parent = context.spawn(Parent(), "parent")

      parent ! CreateChild("test")
      parent ! CreateChild("test")
      parent ! CreateChild("test2")
      parent ! CreateChild("test3")
      parent ! TellChild("test3", "lalala")
      parent ! TellChild("test4", "lalala")

      Behaviors.empty
    }

    val system = ActorSystem(userGuardBehavior, "Child")
    Thread.sleep(500)
    system.terminate()
  }

  demo()
}
