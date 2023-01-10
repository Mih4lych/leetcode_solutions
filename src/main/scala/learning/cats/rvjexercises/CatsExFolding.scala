package learning.cats.rvjexercises

import cats.Monoid
import cats.instances.int._

object CatsExFolding extends App {
  //methods in term of foldLeft
  object ListEx {
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      list.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      list.foldLeft(List.empty[B])((acc, a) => acc ::: f(a))
    }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
      list.foldRight(List.empty[A])((a, acc) => if (predicate(a)) a :: acc else acc)
    }

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import ListEx._

  val testList = List(1, 2, 3, 5, 20)

  println(map(testList)(_ + 1))
  println(flatMap(testList)(a => List(a, a + 1)))
  println(filter(testList)(_ >= 5))
  println(combineAll(testList))
}
