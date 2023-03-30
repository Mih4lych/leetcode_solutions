package learning.fs2.udemy
import cats.effect.{IO, IOApp}
import fs2._

object Task02 extends IOApp.Simple {
  def lettersItter: IO[Stream[Pure, Char]] = {
    IO(
      Stream
        .iterate('a')(c => (c + 1).toChar)
        .take(26)
    )
  }
  def lettersUnfold: IO[Stream[Pure, Char]] = {
    IO(
      Stream
        .unfold('a')(c => if (c > 'z') None else Some(c, (c + 1).toChar))
        .take(100)
    )
  }

  def myIterate[A](init: A)(next: A => A): Stream[Pure, A] = {
    Stream.unfold(init)(a => Option((a, next(a))))
  }

  override def run: IO[Unit] = lettersUnfold.map(s => println(s.toList))
}
