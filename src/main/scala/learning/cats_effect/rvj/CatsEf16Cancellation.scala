package learning.cats_effect.rvj

import cats.effect.{IO, IOApp, MonadCancel}
import cats.syntax.flatMap._
import cats.syntax.functor._
import learning.cats_effect.utils.general._

import scala.concurrent.duration._

object CatsEf16Cancellation extends IOApp.Simple {
  /**
   * Exercise - generalize a piece of code (the auth-flow example from the Cancellation lesson)
   */
  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

  def inputPassword[F[_], E](implicit mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("Input password:").debug
    _ <- mc.pure("(typing password)").debug
    _ <- unsafeSleep(2.seconds)
    res <- mc.pure("RockTheJVM1!")
  } yield res

  def verifyPassword[F[_], E](pw: String)(implicit mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("verifying...").debug
    _ <- unsafeSleep(2.seconds)
    res <- mc.pure(pw == "RockTheJVM1!")
  } yield res

  def authFlow[F[_], E](implicit mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- mc.onCancel(poll(inputPassword), mc.pure("Authentication timed out. Try again later.").debug.void)
      verified <- verifyPassword(pw) // this is NOT cancelable
      _ <- if (verified) mc.pure("Authentication successful.").debug // this is NOT cancelable
      else mc.pure("Authentication failed.").debug
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow[IO, Throwable].start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] = authProgram
}
