package learning.cats_effect.essentialEffects

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.DurationInt

object Task02TickingClock extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  val tickingClock: IO[Unit] =
    IO.sleep(1.second) >> IO.println(System.currentTimeMillis()) >> tickingClock

}
