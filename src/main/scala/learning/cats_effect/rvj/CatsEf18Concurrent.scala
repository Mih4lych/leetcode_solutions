package learning.cats_effect.rvj

import cats.effect.kernel.Outcome
import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Ref}
import cats.effect.syntax.monadCancel._
import cats.effect.syntax.spawn._
import cats.syntax.flatMap._
import cats.syntax.functor._
import learning.cats_effect.utils.general.{DebugWrapper, unsafeSleep}

import scala.collection.immutable.Queue
import scala.concurrent.duration._

object CatsEf18Concurrent extends IOApp.Simple {
  def eggBoiler(): IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Egg boiling on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- IO("EGG READY!").debug
    } yield ()

    def tickingClock(counter: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- IO(count).debug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  def eggBoilerGeneral[F[_]](implicit concurrent: Concurrent[F]): F[Unit] = {
    def eggReadyNotification(signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- concurrent.pure("Egg boiling on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- concurrent.pure("EGG READY!").debug
    } yield ()

    def tickingClock(counter: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep(1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- concurrent.pure(count).debug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- Ref[F].of(0)
      signal <- Deferred[F, Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }


  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  def ourRacePair[F[_], A, B](ioa: F[A], iob: F[B])(implicit concurrent: Concurrent[F]): F[RaceResult[F, A, B]] = concurrent.uncancelable { poll =>
    for {
      signal <- Deferred[F, EitherOutcome[F, A, B]]
      fiba <- concurrent.guaranteeCase(ioa)(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- concurrent.guaranteeCase(iob)(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel { // blocking call - should be cancelable
        for {
          cancelFibA <- fiba.cancel.start
          cancelFibB <- fibb.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      }
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibb))
      case Right(outcomeB) => Right((fiba, outcomeB))
    }
  }

  abstract class MutexGeneral[F[_]] {
    def acquire: F[Unit]

    def release: F[Unit]
  }
  object MutexGeneral {
    type Signal[F[_]] = Deferred[F, Unit]

    case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

    def unlocked[F[_]]: State[F] = State[F](locked = false, Queue())

    def createSignal[F[_]: Concurrent](): F[Signal[F]] = Deferred[F, Unit]

    def create[F[_]: Concurrent]: F[MutexGeneral[F]] = Ref[F].of(unlocked[F]).map(state => createMutexWithCancellation(state))

    def createMutexWithCancellation[F[_]](state: Ref[F, State[F]])(implicit concurrent: Concurrent[F]): MutexGeneral[F] =
      new MutexGeneral {
        override def acquire: F[Unit] = concurrent.uncancelable { poll =>
          createSignal().flatMap { signal =>

            val cleanup = state.modify {
              case State(locked, queue) =>
                val newQueue = queue.filterNot(_ eq signal)
                State(locked, newQueue) -> release
            }.flatten

            state.modify {
              case State(false, _) => State[F](locked = true, Queue()) -> concurrent.unit
              case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
            }.flatten // modify returns IO[B], our B is IO[Unit], so modify returns IO[IO[Unit]], we need to flatten
          }
        }

        override def release: F[Unit] = state.modify {
          case State(false, _) => unlocked[F] -> concurrent.unit
          case State(true, queue) =>
            if (queue.isEmpty) unlocked[F] -> concurrent.unit
            else {
              val (signal, rest) = queue.dequeue
              State(locked = true, rest) -> signal.complete(()).void
            }
        }.flatten
      }
  }

  override def run: IO[Unit] = eggBoilerGeneral[IO]
}
