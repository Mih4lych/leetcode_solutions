package learning.cats_effect.rvj

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, MonadCancel, Spawn}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._

object CatsEf17Spawn {

  // Spawn = create fibers for any effect
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // creates a fiber
    def never[A]: F[A] // a forever-suspending effect
    def cede: F[Unit] // a "yield" effect

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[ // fundamental racing
      (Outcome[F, E, A], Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  def simpleRace[F[_], A, B](ioa: F[A], iob: F[B])(implicit spawn: Spawn[F]): F[Either[A, B]] =
    spawn.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> effectA.map(a => a.asLeft[B])
        case Errored(e) => fibB.cancel.flatMap(_ => spawn.raiseError(e))
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> effectB.map(b => b.asRight[A])
        case Errored(e) => fibA.cancel.flatMap(_ => spawn.raiseError(e))
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
    }
}
