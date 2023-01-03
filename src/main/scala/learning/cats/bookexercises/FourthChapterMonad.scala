package learning.cats.bookexercises

import cats.{Eval, MonadError}
import cats.data.{Reader, Writer}
import cats.instances.try_._
import cats.instances.either._
import cats.instances.vector._
import cats.instances.string._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.writer._

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Try

object FourthChapterMonad extends App {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  type TestEither[A] = Either[Throwable, A]

  println(validateAdult[TestEither](18))
  println(validateAdult[TestEither](8))

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  println(foldRight((1 to 10000).toList, 0L)(_ + _))

  type Logger[A] = Writer[Vector[String], A]
  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logger[Int] = {
    for {
      res <-
        if (n == 0) 1.pure[Logger]
        else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"res $n $res").tell
    } yield res
  }

  println(factorial(10).written.mkString("\n"))

  implicit val context: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  println(Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )).map(_.map(_.written)), 5.seconds).mkString("\n"))

  final case class Db(
                       usernames: Map[Int, String],
                       passwords: Map[String, String]
                     )
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(
                  userId: Int,
                  password: String): DbReader[Boolean] =
    for {
      user <- findUsername(userId)
      isValid <- user.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield isValid

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  // res7: cats.package.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db))
  // res8: cats.package.Id[Boolean] = false
}
