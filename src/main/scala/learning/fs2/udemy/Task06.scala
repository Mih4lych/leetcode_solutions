package learning.fs2.udemy

import cats.effect.{IO, IOApp}
import fs2.concurrent.SignallingRef
import fs2.Stream

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Task06 extends IOApp.Simple {
  type Temperature = Double

  def createTemperatureSensor(alarm: SignallingRef[IO, Temperature], threshold: Temperature): Stream[IO, Nothing] = {
    Stream
      .repeatEval(IO(Random.between(-40.0, 40.0)))
      .evalTap(IO.println)
      .evalMap(t => if (t > threshold) alarm.set(t) else IO.unit)
      .drain
  }

  def createCooler(alarm: SignallingRef[IO, Temperature]): Stream[IO, Nothing] = {
    alarm
      .discrete
      .evalMap(t => IO.println(f"Need to use cooler, temperature is too high $t%.1f"))
      .drain
  }

  val threshold = 20.0
  val initialTemperature = 20.0

  val program =
    Stream.eval(SignallingRef[IO, Temperature](initialTemperature)).flatMap { signal =>
      createTemperatureSensor(signal, threshold).concurrently(createCooler(signal))
    }.interruptAfter(3.seconds).compile.drain

  override def run: IO[Unit] = program
}
