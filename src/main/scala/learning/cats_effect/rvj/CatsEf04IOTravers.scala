package learning.cats_effect.rvj

import cats.Traverse
import cats.effect.{IO, IOApp}
import cats.instances.list._
import cats.syntax.traverse._

object CatsEf04IOTravers {
  def sequence[A](listOfIO: List[IO[A]]): IO[List[A]] = listOfIO.traverse(identity)
  def sequence_v2[F[_] : Traverse, A](listOfIO: F[IO[A]]): IO[F[A]] = listOfIO.traverse(identity)
}
