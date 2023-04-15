package learning.fs2.udemy

import cats.Order
import cats.effect.std.Queue
import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.concurrent.{Channel, SignallingRef, Topic}
import fs2.Stream

import java.time.LocalDateTime
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

  object Ex3 {
    case class CarPosition(carId: Long, lat: Double, lng: Double)

    def createCar(carId: Long, topic: Topic[IO, CarPosition]): Stream[IO, Nothing] = {
      Stream
        .repeatEval(IO(CarPosition(carId = carId, lat = Random.between(-90.0, 90.0), lng = Random.between(-100.0, 100.0))))
        .metered(1.second)
        .through(topic.publish)
        .drain
    }

    def createGoogleMapUpdate(topic: Topic[IO, CarPosition]): Stream[IO, Nothing] = {
      topic
        .subscribe(10)
        .evalMap(pos => IO.println(f"Car ${pos.carId} pos ${pos.lat}%.2f ${pos.lng}%.2f"))
        .drain
    }

    def createDriverNotifier(topic: Topic[IO, CarPosition], shouldNotify: CarPosition => Boolean, notify: CarPosition => IO[Unit]): Stream[IO, Nothing] = {
      topic
        .subscribe(10)
        .evalMap(pos => if (shouldNotify(pos)) notify(pos) else IO.unit)
        .drain
    }

    val program =
      Stream.eval(Topic[IO, CarPosition]).flatMap { topic =>
        (Stream.range(1, 10).map(createCar(_, topic)) ++
          Stream(createGoogleMapUpdate(topic), createDriverNotifier(topic, c => c.carId % 2 == 0, c => IO.println(s"Car ${c.carId} has an even number"))))
          .parJoinUnbounded
      }.interruptAfter(3.seconds).compile.drain
  }

  object Ex4 {
    trait Controller {
      def postAccount(customerId: Long, accountType: String, creationDate: LocalDateTime): IO[Unit]
    }

    class Server(controller: Controller) {
      def start(): IO[Nothing] = {
        val program =
          for {
            randomWait <- IO(math.abs(Random.nextInt()) % 500)
            _          <- IO.sleep(randomWait.millis)
            _          <- controller.postAccount(Random.between(1L, 1000L), (if (Random.nextBoolean()) "ira" else "brokerage"), LocalDateTime.now())
          } yield ()

        program.foreverM
      }
    }

    object PrintController extends Controller {
      override def postAccount(customerId: Long, accountType: String, creationDate: LocalDateTime): IO[Unit] = {
        IO.println(s"customer $customerId type $accountType date $creationDate")
      }
    }

    case class CreateAccountData(customerId: Long, accountType: String, creationDate: LocalDateTime)
    class QueueController(queue: Queue[IO, CreateAccountData]) extends Controller {
      override def postAccount(customerId: Long, accountType: String, creationDate: LocalDateTime): IO[Unit] =
        queue.offer(CreateAccountData(customerId, accountType, creationDate))
    }

    val program = new Server(PrintController).start()

    val program2 = Stream.eval(Queue.unbounded[IO, CreateAccountData]).flatMap { queue =>
      val consumer =
        Stream
          .fromQueueUnterminated(queue)
          .evalMap(IO.println)
          .drain

      val server = Stream.eval(new Server(new QueueController(queue)).start())

      consumer.merge(server)
    }.interruptAfter(3.seconds).compile.drain
  }
  override def run: IO[Unit] = Ex4.program2
}
