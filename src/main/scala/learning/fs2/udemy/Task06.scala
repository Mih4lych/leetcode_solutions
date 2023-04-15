package learning.fs2.udemy

import cats.Order
import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.concurrent.{Channel, SignallingRef}
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

  //ex2
  object Ex2 {
    sealed trait Measurement
    case class Temperature(value: Double) extends Measurement
    case class Humidity(value: Double) extends Measurement

    implicit val ordHum: Order[Humidity] = Order.by(_.value)
    implicit val ordTem: Order[Temperature] = Order.by(_.value)

    def createTemperatureSensor(alarm: Channel[IO, Measurement], threshold: Temperature): Stream[IO, Nothing] = {
      Stream
        .repeatEval(IO(Temperature(Random.between(-40.0, 40.0))))
        .evalTap(t => IO.println(f"${t.value}%.1f temperature"))
        .evalMap(t => if (t > threshold) alarm.send(t) else IO.unit)
        .metered(300.millis)
        .drain
    }

    def createHumiditySensor(alarm: Channel[IO, Measurement], threshold: Humidity): Stream[IO, Nothing] = {
      Stream
        .repeatEval(IO(Humidity(Random.between(0.0, 100.0))))
        .evalTap(t => IO.println(f"${t.value}%.1f humidity"))
        .evalMap(t => if (t > threshold) alarm.send(t) else IO.unit)
        .metered(100.millis)
        .drain
    }

    def createCooler(alarm: Channel[IO, Measurement]): Stream[IO, Nothing] = {
      alarm
        .stream
        .evalMap {
          case Temperature(value) => IO.println(s"temperature too high $value")
          case Humidity(value) => IO.println(s"Humidity too high $value")
        }
        .drain
    }

    val program =
      Stream.eval(Channel.unbounded[IO, Measurement]).flatMap { channel =>
        Stream(createTemperatureSensor(channel, Temperature(20.0)), createHumiditySensor(channel, Humidity(50.0)))
          .parJoinUnbounded
          .concurrently(createCooler(channel))
      }.interruptAfter(5.seconds).compile.drain
  }

  override def run: IO[Unit] = Ex2.program
}
