package learning.cats_effect.rvj

import cats.effect.kernel.Outcome
import cats.effect.{Deferred, Fiber, IO, IOApp, Ref}
import cats.syntax.traverse._
import cats.syntax.either._
import learning.cats_effect.utils.DebugWrapper

import scala.concurrent.duration._

object CatsEf11Deferred extends IOApp.Simple {

  def consumer[A](signal: Deferred[IO, A]): IO[Unit] = {
    for {
      value <- signal.get
      _ <- IO.pure(value).debug >> IO.sleep(3.second)
      _ <- consumer(signal)
    } yield ()
  }

  def producer[A](signal: Deferred[IO, A], value: A): IO[Unit] = {
    for {
      _ <- IO.sleep(2.second)
      _ <- signal.complete(value).debug
      _ <- producer(signal, value)
    } yield ()
  }

  def program[A](value: A): IO[Unit] = {
    for {
      signal <- Deferred[IO, A]
      _ <- producer(signal, value)
      _ <- consumer(signal)
    } yield ()
  }

  /**
   *  Exercises:
   *  - (medium) write a small alarm notification with two simultaneous IOs
   *    - one that increments a counter every second (a clock)
   *    - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   *  - (mega hard) implement racePair with Deferred.
   *    - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   *    - start two fibers, one for each IO
   *    - on completion (with any status), each IO needs to complete that Deferred
   *      (hint: use a finalizer from the Resources lesson)
   *      (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *    - what do you do in case of cancellation (the hardest part)?
   */
  // 1
  def alarm(): IO[Unit] = {
    def timer(time: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = {
      for {
        _ <- IO.pure(System.currentTimeMillis()).debug >> IO.sleep(1.second)
        curTime <- time.updateAndGet(_ + 1)
        _ <- if (curTime == 10) IO("Wake up").debug >> signal.complete(curTime) else timer(time, signal)
      } yield ()
    }

    def notification(signal: Deferred[IO, Int]): IO[Unit] = {
      for {
        _ <- signal.get
        _ <- IO("time's up").debug >> IO.unit
      } yield ()
    }

    for {
      time <- IO.ref(0)
      signal <- IO.deferred[Int]
      fibTimer <- timer(time, signal).start
      fibNote <- notification(signal).start
      _ <- fibTimer.join
      _ <- fibNote.join
    } yield ()
  }

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])  // (loser fiber, winner result)
  ]

  type Winner[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = {
    IO.uncancelable { poll =>
      for {
        signal <- IO.deferred[Winner[A, B]]
        fibA <- ioa.guaranteeCase(outcomeA => signal.complete(outcomeA.asLeft).void).start
        fibB <- iob.guaranteeCase(outcomeB => signal.complete(outcomeB.asRight).void).start
        res <- poll(signal.get).onCancel {
          for {
            cancelFibA <- fibA.cancel.start
            cancelFibB <- fibB.cancel.start
            _ <- cancelFibA.join
            _ <- cancelFibB.join
          } yield ()
        }
      } yield res match {
        case Left(value) => Left((value, fibB))
        case Right(value) => Right((fibA, value))
      }
    }


  }

  override def run: IO[Unit] = alarm()
}
