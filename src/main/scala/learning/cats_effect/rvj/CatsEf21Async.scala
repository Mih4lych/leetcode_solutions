package learning.cats_effect.rvj

import cats.effect.{Async, Concurrent, Sync, Temporal}

import scala.concurrent.ExecutionContext

object CatsEf21Async {

  // Async - asynchronous computations, "suspended" in F
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental description of async computations
    def executionContext: F[ExecutionContext]

    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]

    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(kb => map(pure(cb(kb)))(_ => None))

    def never[A]: F[A] = async_(_ => ())
  }

  def firstEffect[F[_] : Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)

  def secondEffect[F[_] : Sync, A](a: A): F[A] = Sync[F].pure(a)

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] = for {
    first <- firstEffect(a)
    second <- secondEffect(a)
  } yield (first, second)
}
