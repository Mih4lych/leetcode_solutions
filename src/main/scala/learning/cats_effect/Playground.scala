package learning.cats_effect

import cats.effect.{IO, IOApp}

object Playground extends IOApp.Simple {
  override def run: IO[Unit] = {
    IO
      .raiseError(new RuntimeException("test"))
      .onError(_ => IO.println("test onError"))
      .void
  }
}
