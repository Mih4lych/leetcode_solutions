package learning.cats.rvjexercises

import cats.{Functor, Semigroupal}

object CatsExApply extends App {
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      map(product(tuple._1, tuple._2)){case (a, b) => f(a, b)}
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T]
  }
}
