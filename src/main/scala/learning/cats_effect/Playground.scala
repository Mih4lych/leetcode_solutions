package learning.cats_effect

import cats.data.OptionT
import cats.syntax.apply._
import cats.syntax.option._
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._

object Playground extends IOApp.Simple {
  val testOnError = IO
    .raiseError(new RuntimeException("test"))
    .onError(_ => IO.println("test onError"))
    .void

  val test1 = OptionT(IO(println("asd")) *> IO(none[Int]))
  val test2 = OptionT.liftF(IO(println("asd"))) *> OptionT(IO(none[Int]))

  val testWithRef1 = Ref[IO].of(1).flatMap { ref =>
    (for {
      _ <- OptionT.liftF(ref.update(_ + 1) *> IO(none[Int]))
    } yield ref).value
  }

  val testWithRef2 = Ref[IO].of(1).flatMap { ref =>
    (for {
      _ <- OptionT.liftF(ref.update(_ + 1)) *> OptionT(IO(none[Int]))
    } yield ref).value
  }

  override def run: IO[Unit] = test1.value.void

}
