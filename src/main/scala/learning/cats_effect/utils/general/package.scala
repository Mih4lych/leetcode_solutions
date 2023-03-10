package learning.cats_effect.utils

import cats.Functor
import cats.syntax.functor._

package object general {
  implicit class DebugWrapper[F[_], A](cont: F[A]) {
    def debug(implicit fun: Functor[F]): F[A] = cont.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }
  }
}
