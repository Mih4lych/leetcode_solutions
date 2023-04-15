package learning.akka.rvj

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt

object Ex2State extends App {
  object WordCounter {
    def apply(): Behavior[String] = active(0)

    def active(counter: Int): Behavior[String] = Behaviors.setup { context =>
      Behaviors.receiveMessage { m =>
        val curCount = m.split(" ").length

        context.log.info(s"current $curCount all ${counter + curCount}")

        active(counter + curCount)
      }
    }
  }

  val test = ActorSystem(WordCounter(), "Counter")

  test ! "asd asd asd asd "
  test ! "asd asd asd asd asd"
  test ! "asd asd"

  Thread.sleep(500)
  test.terminate()
}
