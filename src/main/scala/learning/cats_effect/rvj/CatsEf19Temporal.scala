package learning.cats_effect.rvj

import cats.effect.Temporal
import cats.syntax.flatMap._

import scala.concurrent.duration.FiniteDuration

object CatsEf19Temporal {
  def timeout[F[_], A](io: F[A], duration: FiniteDuration)(implicit temporal: Temporal[F]): F[A] = {
    val timeoutEffect = temporal.sleep(duration)
    val result = temporal.race(io, timeoutEffect)

    result.flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException("Computation timed out."))
    }
  }
}
