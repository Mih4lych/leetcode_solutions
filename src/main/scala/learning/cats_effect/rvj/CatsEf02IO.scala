package learning.cats_effect.rvj

import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global

object CatsEf02IO extends App {
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a

  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))

  def convert[A, B](io: IO[A], value: B): IO[B] = io.map(_ => value)

  def asUnit[A](io: IO[A]): IO[Unit] = io.map(_ => ())

  def sumIO(n: Int): IO[Int] = {
    if (n <= 0) IO.pure(0)
    else for {
      cur <- IO(n)
      prev <- sumIO(n - 1)
    } yield cur + prev
  }

  def fibonacci(n: Int): IO[BigInt] = {
    if (n < 2) IO.pure(1)
    else for {
      prev <- IO.defer(fibonacci(n - 2))
      last <- IO.defer(fibonacci(n - 1))
    } yield prev + last
  }

  println(fibonacci(20).unsafeRunSync())
}
