package learning.akka.rvj

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object Ex01Basics extends App {

  object Person {
    def happy(): Behavior[String] = Behaviors.receive { (context, message) =>
      context.log.info(s"Happy message: $message")

      if (message.equals("Akka is bad")) sad()
      else Behaviors.same
    }

    def sad(): Behavior[String] = Behaviors.receive { (context, message) =>
      context.log.info(s"Sad message: $message")

      if (message.equals("Akka is awesome")) happy()
      else Behaviors.same
    }
  }

  def demo(): Unit = {
    val akkaSystem = ActorSystem(Person.happy(), "Test")

    akkaSystem ! "Akka is bad"
    akkaSystem ! "Akka is bad"
    akkaSystem ! "Akka is awesome"
    Thread.sleep(1000)

    akkaSystem.terminate()
  }

  demo()

}
