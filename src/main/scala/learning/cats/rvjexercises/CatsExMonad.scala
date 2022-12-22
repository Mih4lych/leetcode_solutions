package learning.cats.rvjexercises

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import cats.Monad
import cats.data.EitherT
import cats.instances.future._
import cats.instances.option._
import cats.instances.try_._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.annotation.tailrec
import scala.reflect.ClassTag
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

  //Custom monad

  //monad for identity
  type Identity[T] = T

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = {
      f(a) match {
        case Right(b) => b
        case Left(a) => tailRecM(a)(f)
      }
    }
  }

  val identityMonad = Monad[Identity]
  val identityInt = 5.pure[Identity]
  println(identityInt.flatMap(int => s"teas $int"))

  //monad for tree
  sealed trait Tree[+A]

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def leaf[A](value: A): Leaf[A] = Leaf(value)
    def branch[A](left: Tree[A], right: Tree[A]): Branch[A] = Branch(left, right)
  }

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = {
      Tree.leaf(x)
    }

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      /*fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Tree.branch(flatMap(left)(f), flatMap(right)(f))
      }*/

      @tailrec
      def tailRecFlatMap(todo: List[Tree[A]], expanded: List[Tree[A]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else {
          todo.head match {
            case Leaf(value) => tailRecFlatMap(todo.tail, expanded, f(value) :: done)
            case node @ Branch(left, right) =>
              if (expanded.isEmpty || expanded.head != node) {
                tailRecFlatMap(left :: right :: todo, node :: expanded, done)
              }
              else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Tree.branch(newLeft, newRight)

                tailRecFlatMap(todo.tail, expanded.tail, newBranch :: done)
              }
          }
        }
      }

      tailRecFlatMap(List(fa), List(), List())
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(tree: Tree[Either[A, B]]): Tree[B] = tree match {
        case Leaf(Left(a)) => stackRec(f(a))
        case Leaf(Right(b)) => Tree.leaf(b)
        case Branch(left, right) => Tree.branch(stackRec(left), stackRec(right))
      }

      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: List[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else {
          todo.head match {
            case Leaf(Left(value)) => tailRec(f(value) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Tree.leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (expanded.isEmpty || expanded.head != node) {
                tailRec(left :: right :: todo, node :: expanded, done)
              }
          else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Tree.branch(newLeft, newRight)

                tailRec(todo.tail, expanded.tail, newBranch :: done)
              }
          }
        }
      }

      tailRec(List(f(a)), List(), List())
    }
  }

  val example: Tree[Either[Int, String]] =
    Branch(
      Branch(
        Leaf(Left(1)),
        Leaf(Left(2))),
      Branch(
        Leaf(Left(1)),
        Leaf(Left(2))),
    )

  def fun(x: Int): Tree[Either[Int, String]] =
    if (x == 0) example
    else Leaf(Right((x * 10).toString))

  println(Monad[Tree].tailRecM(0)(fun))

  //Monad Transformer

  /*
     We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
     We measure bandwidth in units.
     We want to allocate TWO of our servers to cope with the traffic spike.
     We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
    */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // hint: call getBandwidth twice, and combine the result
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    first <- getBandwidth(s1)
    second <- getBandwidth(s2)
  } yield first + second > 250

  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(s) => Left(s)
      case Right(true) => Right("all is fine")
      case Right(false) => Right("all is wrong")
    }

  val resultFuture = generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value
  resultFuture.foreach(println)
}
