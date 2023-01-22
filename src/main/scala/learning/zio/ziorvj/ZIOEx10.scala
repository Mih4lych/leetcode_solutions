package learning.zio.ziorvj

import zio._
import utils._

import java.util.concurrent.TimeUnit

object ZIOEx10 extends ZIOAppDefault{

  /**
   * Exercises
   */
  // 1 - refactor this code using a Ref
  def tickingClockPure(): UIO[Unit] = {
    val ticks = Ref.make(0L)
    // print the current time every 1s + increase a counter ("ticks")
    def tickingClock: UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      ticksRef <- ticks
      ticksCount <- ticksRef.updateAndGet(_ + 1)
      _ <- ZIO.succeed(ticksCount)
      _ <- tickingClock
    } yield ()

    // print the total ticks count every 5s
    def printTicks: UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      ticksRef <- ticks
      tickCount <- ticksRef.get
      _ <- ZIO.succeed(s"TICKS: $tickCount").debugThread
      _ <- printTicks
    } yield ()

    (tickingClock zipPar printTicks).unit
  }

  // 1 - refactor this code using a Ref
  def tickingClockPureFinal(): UIO[Unit] = {
    // print the current time every 1s + increase a counter ("ticks")
    def tickingClock(ticks: Ref[Int]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    // print the total ticks count every 5s
    def printTicks(ticks: Ref[Int]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      tickCount <- ticks.get
      _ <- ZIO.succeed(s"TICKS: $tickCount").debugThread
      _ <- printTicks(ticks)
    } yield ()

    Ref.make(0).flatMap(ref => tickingClock(ref) zipPar printTicks(ref)).unit
  }

  def run = tickingClockPure()
}
