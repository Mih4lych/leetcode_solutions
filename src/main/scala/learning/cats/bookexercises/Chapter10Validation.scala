package learning.cats.bookexercises

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

object Chapter10Validation extends App {

  final case class Check[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and(that: Check[E, A])(implicit semigroup: Semigroup[E]): Check[E, A] = {
      Check { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e1), _) => e1.asLeft
          case (_, Left(e2)) => e2.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
    }
  }

}
