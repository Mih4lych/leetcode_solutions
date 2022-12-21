package learning.cats

import cats.Eq
import cats.Functor
import cats.instances.list._
import cats.syntax.functor._
object CatsPlayground extends App {
  val list = List(1, 2, 3, 5)

  println(list.fmap(_ + 1))
}
