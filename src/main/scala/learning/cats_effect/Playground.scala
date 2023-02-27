package learning.cats_effect

import cats.effect.{IO, IOApp}

object Playground extends IOApp.Simple {
  override def run: IO[Unit] =
    IO.println("Let's go!!!")
}
