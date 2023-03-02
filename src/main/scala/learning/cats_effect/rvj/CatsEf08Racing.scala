package learning.cats_effect.rvj

import cats.effect.IO
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.syntax.either._

import scala.concurrent.duration.FiniteDuration

object CatsEf08Racing {
  //1 - timeout
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(io, IO.sleep(duration)).flatMap {
      case Left(value) => IO(value)
      case Right(_) => IO.raiseError(new RuntimeException("timeout"))
    }

  //2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fib)) => fib.join.flatMap {
        case Succeeded(res) => res.map(_.asRight[A])
        case Errored(er) => IO.raiseError(er)
        case Canceled() => IO.raiseError(new RuntimeException("canceled"))
      }
      case Right((fib, _)) => fib.join.flatMap {
        case Succeeded(res) => res.map(_.asLeft[B])
        case Errored(er) => IO.raiseError(er)
        case Canceled() => IO.raiseError(new RuntimeException("canceled"))
      }
    }

  //3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fib)) =>
        outcomeA match {
          case Succeeded(res) => fib.cancel >> res.map(_.asLeft[B])
          case Errored(er) => fib.cancel >> IO.raiseError(er)
          case Canceled() => fib.join.flatMap {
            case Succeeded(res) => res.map(_.asRight[A])
            case Errored(er) => IO.raiseError(er)
            case Canceled() => IO.raiseError(new RuntimeException("no winner"))
          }
        }
      case Right((fib, outcomeB)) =>
        outcomeB match {
          case Succeeded(res) => fib.cancel >> res.map(_.asRight[A])
          case Errored(er) => fib.cancel >> IO.raiseError(er)
          case Canceled() => fib.join.flatMap {
            case Succeeded(res) => res.map(_.asLeft[B])
            case Errored(er) => IO.raiseError(er)
            case Canceled() => IO.raiseError(new RuntimeException("no winner"))
          }
        }
    }
}
