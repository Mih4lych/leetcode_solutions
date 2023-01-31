package learning.cats

import cats.data.{Const, Validated}
import cats.{Eval, FunctorFilter, Monad, Order, Semigroupal}
import cats.instances.list._
import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.order._
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

  val testEval = Eval.now {
    println("asd")
    42
  }.map { _ =>
    println("asddd")
    59
  }

  //println(testEval.value)
  type ErrorOr[A] = Validated[Int, A]
  val testSemigroupal = Semigroupal[ErrorOr].product(
    Validated.invalid(10),
    Validated.invalid(20)
  )

  println(testSemigroupal)

  case class Cat(name: String, years: Int)
  object Cat {
    implicit val testOrder: Order[Cat] = (x, y) => x.years.compare(y.years)
  }

  println(Cat("asd", 11) >= Cat("ddd", 21))

  println(List(1, 2, 3).fproduct(_ * 4)) //result List((1,4), (2,8), (3,12))

  println(List(1, 2, 3) >> List(3, 4)) //flatmap(_ => List(3, 4))
  println(List(1, 2, 3) >>= {x => List(x, x * 10)}) //flatmap(x => List(x, x * 10))

  val testFuncFilter = FunctorFilter[List]

  println(testFuncFilter.mapFilter(List(1, 2, 3)){x => if (x % 2 == 0) Some(x) else None})

  val testConst = Const(1)
}
