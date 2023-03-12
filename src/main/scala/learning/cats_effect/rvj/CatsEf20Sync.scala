package learning.cats_effect.rvj

import cats.effect.{IO, IOApp, MonadCancel, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._

import java.io.{BufferedReader, InputStreamReader}
import scala.io.StdIn

object CatsEf20Sync extends IOApp.Simple {
  trait MySync[F[_]] extends MonadCancel[F, Throwable] {
    def delay[A](thunk: => A): F[A]
    def blocking[A](thunk: => A): F[A]

    def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  /**
   * Exercise - write a polymorphic console
   */
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]

    def readLine(): F[String]
  }

  object Console {
    def make[F[_]](implicit sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        def println[A](a: A): F[Unit] =
          sync.blocking(out.println(a))

        def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in))
          sync.blocking(bufferedReader.readLine())
        }
      }
    }
  }

  def consoleReader(): IO[Unit] = for {
    console <- Console.make[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Hi $name, nice to meet you!")
  } yield ()

  override def run: IO[Unit] = consoleReader()
}
