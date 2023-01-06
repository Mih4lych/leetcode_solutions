package learning.cats.rvjexercises

import cats.{Monad, Semigroupal}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroupal._

object CatsExSemigroupal extends App {
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    //monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  println(productWithMonads(List(1, 2, 3), List("a", "b", "c")))

  //Semigroupal[List] with zip method

  val listSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  println(listSemigroupal.product(List(1, 2, 3), List("a", "b")))
}
