package learning.cats.bookexercises

import cats.Monoid
import cats.instances.int._
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.traverse._

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Chapter9MapReduce extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  def foldMap[A, B : Monoid](seq: Vector[A])(f: A => B): B =
    seq.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

  println(foldMap(Vector(1, 2, 3))(identity))
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    values.grouped(values.length / 8).map(batch => Future(foldMap(batch)(func))).toVector.sequence.map(foldMap(_)(identity))
  }

  def parallelFoldMapByCats[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    values
      .grouped((1.0 * values.size / 8).ceil.toInt)
      .toVector
      .traverse(vector => Future(vector.foldMap(func)))
      .map(_.combineAll)
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)
  Await.result(result, 1.second)

  println(result)

  val resultCats: Future[Int] =
    parallelFoldMapByCats((1 to 1000000).toVector)(identity)
  Await.result(resultCats, 1.second)

  println(resultCats)
}
