package learning.cats_effect.essentialEffects

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationLong, FiniteDuration}

object Task01Timing extends App {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  object MyIO {
    def putStr(s: => String): MyIO[Unit] =
      MyIO(() => println(s))
  }

  val clock: MyIO[Long] =
    MyIO(() => System.currentTimeMillis())

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    for {
      begin <- clock
      a <- action
      end <- clock
    } yield ((end - begin).millisecond, a)

  val timedHello = Task01Timing.time(MyIO.putStr("hello"))
  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }

}
