package learning.cats_effect.rvj

import cats.effect.{Deferred, IO, Ref}

object CatsEf15CyclicBarrier {
  abstract class CB {
    def await: IO[Unit]
  }
  object CB {
    case class Await(remainingCount: Int, signal: Deferred[IO, Unit])

    def apply(n: Int): IO[CB] = for {
        signal <- Deferred[IO, Unit]
        ref <- Ref[IO].of[Await](Await(n, signal))
      } yield new CB {
      override def await: IO[Unit] = Deferred[IO, Unit].flatMap { newSignal =>
        ref.modify {
          case Await(1, signal) => Await(n, newSignal) -> signal.complete(()).void
          case Await(remainingCount, signal) =>
            Await(remainingCount - 1, signal) -> signal.get
        }.flatten
      }
    }
  }
}
