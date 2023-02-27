package learning.cats_effect.rvj

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object CatsEf03IOException {
  def option2IO[A](option: Option[A])(isEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO(value)
      case None        => IO.raiseError(isEmpty)
    }

  def try2IO[A](_try: Try[A]): IO[A] =
    _try match {
      case Success(value) => IO(value)
      case Failure(ex)    => IO.raiseError(ex)
    }

  def either2IO[A <: Throwable, B](either: Either[A, B]): IO[B] =
    either match {
      case Right(value) => IO(value)
      case Left(er)     => IO.raiseError(er)
    }

  def handlerIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)
  def handlerIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO(_))

}
