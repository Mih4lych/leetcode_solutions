package learning.ziorvj

import scala.util.{Failure, Success, Try}
import zio._

object ZIOEx3 {
  /**
   * Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
   * using fold and foldZIO
   */

  def try2ZIO[A](aTry: Try[A]): Task[A] =
    aTry match {
      case Success(value) =>
        ZIO.succeed(value)
      case Failure(exception) =>
        ZIO.fail(exception)
    }
  def try2ZIO_v2[A](aTry: Try[A]): Task[A] =
    aTry.fold(fa => ZIO.fail(fa), fb => ZIO.succeed(fb))

  def either2ZIO[E, A](aEither: Either[E, A]): IO[E, A] = {
    aEither.fold(fa => ZIO.fail(fa), fb => ZIO.succeed(fb))
  }

  def option2ZIO[A](aOption: Option[A]): IO[Option[Nothing], A] = {
    aOption match {
      case Some(value) => ZIO.succeed(value)
      case _ => ZIO.fail(None)
    }
  }

  def either[E, A](aZIO: IO[E, A]): UIO[Either[E, A]] = {
    aZIO.fold(fa => Left(fa), fb => Right(fb))
  }

  def absolve[E, A](aZIOEither: UIO[Either[E, A]]): IO[E, A] = {
    aZIOEither.flatMap(either2ZIO)
  }
}
