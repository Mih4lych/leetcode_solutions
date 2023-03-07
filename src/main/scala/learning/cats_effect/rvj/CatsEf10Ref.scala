package learning.cats_effect.rvj

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._
import learning.cats_effect.utils.DebugWrapper

import scala.concurrent.duration._

object CatsEf10Ref extends IOApp.Simple {
  /**
   * Exercise
   */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock(): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1) // not thread safe
      _ <- tickingClock()
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock(), printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    val ticks = IO.ref(0L)

    def tickingClock(ref: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ref.update(_ + 1)
      _ <- tickingClock(ref)
    } yield ()

    def printTicks(ref: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      curTicks <- ref.get
      _ <- IO(s"TICKS: $curTicks").debug
      _ <- printTicks(ref)
    } yield ()

    for {
      ref <- ticks
      _ <- (tickingClock(ref), printTicks(ref)).parTupled
    } yield ()
  }

  override def run: IO[Unit] = tickingClockPure()
}
