package learning.cats.rvjexercises

import cats.Apply

object CatsExFlatMap {
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](mf: M[A => B])(ma: M[A]): M[B] =
      flatMap(mf)(f => map(ma)(f))
  }
}
