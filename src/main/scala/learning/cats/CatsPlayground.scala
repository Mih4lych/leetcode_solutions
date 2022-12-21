package learning.cats

import cats.Monad
import cats.instances.list._
import cats.instances.either._
import cats.syntax.functor._
object CatsPlayground extends App {
  val list = List(1, 2, 3, 5)

  println(list.fmap(_ + 1))

  type TestType[T] = Either[String, T]
  val testTypeMonad = Monad[TestType]
  val test = testTypeMonad.pure("asd")

  println(
    test match {
      case Left(value) => s"left with $value"
      case Right(value) => s"right with $value"
    }
  )
}
