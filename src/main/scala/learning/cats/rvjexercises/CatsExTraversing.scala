package learning.cats.rvjexercises

import cats.{Applicative, Foldable, Monad}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object CatsExTraversing extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers = List("asdasda.asd", "vsfsdasd.asdsds/qweqwewqe")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidth: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidth2: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])){(acc, string) =>
    for {
      list <- acc
      num <- getBandwidth(string)
    } yield num :: list
  }.map(_.reverse)

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]){(acc, a) =>
      (acc, func(a)).mapN(_ :+ _)
    }
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = {
    listTraverse(list)(identity)
  }

  trait MyTraversing[L[_]] extends Foldable[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    type Identity[T] = T
    def map[A, B](container: L[A])(func: A => B): L[B] = {
      traverse[Identity, A, B](container)(func)
    }
  }
}
