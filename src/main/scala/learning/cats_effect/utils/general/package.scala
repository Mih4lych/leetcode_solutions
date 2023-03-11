package learning.cats_effect.utils

import cats.Functor
import cats.effect.MonadCancel
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration

package object general {
  implicit class DebugWrapper[F[_], A](cont: F[A]) {
    def debug(implicit fun: Functor[F]): F[A] = cont.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }
  }

  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))
}
