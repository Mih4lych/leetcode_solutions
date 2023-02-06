package learning.cats

import cats.data.{Const, NonEmptyList, Validated}
import cats.{Alternative, Applicative, Bifoldable, Contravariant, ContravariantMonoidal, Eval, Foldable, FunctorFilter, Monad, Monoid, Order, Semigroupal, Traverse}
import cats.instances.list._
import cats.instances.either._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.order._
import cats.syntax.functor._
import cats.syntax.semigroupal._
object CatsPlayground extends App {
  val list = List(1, 2, 3, 5)

  println(list.fmap(_ + 1))

  type TestType[T] = Either[String, T]
  val testTypeMonad = Monad[TestType]
  val test = testTypeMonad.pure("asd")

  println(
    test match {
      case Left(value) => s"left with $value"
      case Right(value) => s"right with $value"
    }
  )

  val testEval = Eval.now {
    println("asd")
    42
  }.map { _ =>
    println("asddd")
    59
  }

  //println(testEval.value)
  type ErrorOr[A] = Validated[Int, A]
  val testSemigroupal = Semigroupal[ErrorOr].product(
    Validated.invalid(10),
    Validated.invalid(20)
  )

  println(testSemigroupal)

  case class Cat(name: String, years: Int)
  object Cat {
    implicit val testOrder: Order[Cat] = (x, y) => x.years.compare(y.years)
  }

  println(Cat("asd", 11) >= Cat("ddd", 21))

  println(List(1, 2, 3).fproduct(_ * 4)) //result List((1,4), (2,8), (3,12))

  println(List(1, 2, 3) >> List(3, 4)) //flatmap(_ => List(3, 4))
  println(List(1, 2, 3) >>= {x => List(x, x * 10)}) //flatmap(x => List(x, x * 10))

  val testFuncFilter = FunctorFilter[List]

  println(testFuncFilter.mapFilter(List(1, 2, 3)){x => if (x % 2 == 0) Some(x) else None})

  val testConst = Const(1)

  def reduce[A, B, F[_]](fa: F[A])(f: A => B)
                        (implicit FF: Traverse[F], BB: Monoid[B]): B = {
    val g: A => Const[B, Unit] = { (a: A) => Const((f(a))) }
    val x = FF.traverse[Const[B, *], A, Unit](fa)(g)
    x.getConst
  }

  reduce[Char, Int, List](List('a', 'b', 'c')) { c: Char => c.toInt }

  //a -> Const[Int, Unit](50) => Cont[Int, List[Unit]]

  trait Decoder[A] {
    def decode(in: String): Either[Throwable, A]
  }

  object Decoder {
    def from[A](f: String => Either[Throwable, A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = f(in)
      }
  }

  implicit val decoderAlternative = new Alternative[Decoder] {
    def pure[A](a: A) = Decoder.from(Function.const(Right(a)))

    def empty[A] = Decoder.from(Function.const(Left(new Error("No dice."))))

    def combineK[A](l: Decoder[A], r: Decoder[A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = l.decode(in).orElse(r.decode(in))
      }

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
      new Decoder[B] {
        def decode(in: String) = Applicative[Either[Throwable, *]].ap(ff.decode(in))(fa.decode(in))
      }
  }

  val records: List[(Int, Int)] = List((450000, 3), (770000, 4), (990000, 2), (2100, 4), (43300, 3))
  // records: List[(Int, Int)] = List(
  //   (450000, 3),
  //   (770000, 4),
  //   (990000, 2),
  //   (2100, 4),
  //   (43300, 3)
  // )

  def calculateContributionPerMonth(balance: Int, lifetime: Int) = balance / lifetime

  val result: List[Int] =
    records.map(
      record => record.bimap(
        cents => cents / 100,
        years => 12 * years
      )
    ).map((calculateContributionPerMonth _).tupled)
  // result: List[Int] = List(125, 160, 412, 0, 12)

  println(Bifoldable[(String, *, *)].bifold(("ad", 1, 2.0)))

  println(NonEmptyList.of(1, 2, 3, 4, 5).coflatMap(_.foldLeft(0)(_ + _)))

  /*object ContravariantMonoidalEx {
    case class Predicate[A](run: A => Boolean)

    implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] =
      new ContravariantMonoidal[Predicate] {
        def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))

        def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
          Predicate(x => fa.run(x._1) && fb.run(x._2))

        def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
          Predicate(x => fa.run(f(x)))
      }

    case class Money(value: Long)

    def isEven: Predicate[Long] = Predicate(_ % 2 == 0)

    def isEvenMoney: Predicate[Money] = isEven.contramap(_.value)

    def times2Predicate: Predicate[Long] => Predicate[Long] =
      ContravariantMonoidal[Predicate].liftContravariant((x: Long) => 2 * x)

    def liftMoney: Predicate[Long] => Predicate[Money] =
      ContravariantMonoidal[Predicate].liftContravariant(_.value)

    case class Transaction(value: Money, payee: String)

    def isEvan: Predicate[String] = Predicate(_ == "Evan")

    def isGreaterThan50Dollars: Predicate[Money] = liftMoney(Predicate(_ > 50))

    def isEvenPaymentToEvanOfMoreThan50 =
      (isEvenMoney, isGreaterThan50Dollars, isEvan).contramapN(
        (trans: Transaction) => (trans.value, trans.value, trans.payee))

    isEvenPaymentToEvanOfMoreThan50.run(Transaction(Money(56), "Evan"))
  }*/

  val prints: Eval[Unit] = List(Eval.always(println(1)), Eval.always(println(2))).sequence_

  println(prints.value)

}
