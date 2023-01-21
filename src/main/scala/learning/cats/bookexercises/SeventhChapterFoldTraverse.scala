package learning.cats.bookexercises

import cats.Monoid
import cats.syntax.semigroup._

object SeventhChapterFoldTraverse extends App {
  def mapForList[A, B](list: List[A])(f: A => B): List[B] = {
    list.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
  }

  def sumForList[A](list: List[A])(implicit monoid: Monoid[A]): A = {
    list.foldRight(monoid.empty)(_ |+| _)
  }

  def filterForList[A](list: List[A])(f: A => Boolean): List[A] = {
    list.foldRight(List.empty[A])((a, acc) => if (f(a)) a :: acc else acc)
  }

  def flatMapForList[A, B](list: List[A])(f: A => List[B]): List[B] = {
    list.foldRight(List.empty[B])((a, acc) => f(a) ::: acc)
  }
}
