package learning.fs2.udemy

import cats.effect.{IO, IOApp}
import fs2._

object Task03 extends IOApp.Simple {
  def getAllOddNums: IO[Stream[Pure, Int]] = {
    IO.delay {
      Stream
        .iterate(1)(_ + 1)
        .map(_ * 2 - 1)
    }
  }

  def repeat[A](s: Stream[Pure, A]): Stream[Pure, A] = s ++ repeat(s)

  def unNone[A](s: Stream[Pure, Option[A]]): Stream[Pure, A] =
    s.flatMap(Stream.fromOption(_))
  override def run: IO[Unit] = ???
}
