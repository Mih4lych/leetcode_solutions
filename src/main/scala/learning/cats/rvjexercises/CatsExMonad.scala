package learning.cats.rvjexercises

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import cats.Monad
import cats.instances.future._
import cats.instances.option._
import cats.instances.try_._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.util.Try

object CatsExMonad extends App {
  //all combinations of lists' elements

  val list1 = List(1, 2, 3)
  val list2 = List('a', 'b', 'c')

  val combinations = list1.foldLeft(List.empty[(Int, Char)]) { (acc, nextInt) =>
    acc ::: list2.map((nextInt, _))
  }

  val anotherCombination = for {
    a <- list1
    b <- list2
  } yield (a, b)

  println(combinations)
  println(anotherCombination)

  //same for options
  val opt1 = Option(1)
  val opt2 = Option('a')

  val optComb = for {
    o1 <- opt1
    o2 <- opt2
  } yield (o1, o2)

  println(optComb)

  //same for future
  implicit val execCont: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val f1 = Future(1)
  val f2 = Future('a')

  for {
    o1 <- f1
    o2 <- f2
  } yield (o1, o2)

  //cats Monad[Future]
  val futureMonad = Monad[Future]
  val fPure = futureMonad.pure(1)
  futureMonad.flatMap(fPure)(futureMonad.pure(_, 'a'))

  def getPair[M[_], A, B](containerA: M[A], containerB: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(containerA)(a => monad.flatMap(containerB)(b => monad.pure((a, b))))

  println(getPair(Option(4), Option('a')))
  println(getPair(Try(4), Try('a')))

  //map realisation was completed above (monad.flatMap(containerB)(b => monad.pure((a, b))))

  //implement a shorter version of getPairs using for-comprehensions
  def getPairShorter[M[_] : Monad, A, B](containerA: M[A], containerB: M[B]): M[(A, B)] = {
    for {
      a <- containerA
      b <- containerB
    } yield (a, b)
  }

  println(getPairShorter(Try(4), Try('a')))

  // the service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] = {
    for {
      connection <- service.getConnection(config)
      resp <- service.issueRequest(connection, payload)
    } yield resp
  }

  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M
    provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object HttpServiceOption extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length < 20) Some(s"requested $payload has been accepted")
      else None
    }
  }

  // implement another HttpService with LoadingOr or ErrorOr
  type LoadingOr[T] = Either[String, T]

  object HttpServiceLoad extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      val conOpt = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      conOpt match {
        case Some(value) => Right(value)
        case None => Left("bad connection")
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if (payload.length < 20) Right(s"requested $payload has been accepted")
      else Left("not proper payload")
    }
  }

  println(getResponse(HttpServiceLoad, "asdasd"))
}
