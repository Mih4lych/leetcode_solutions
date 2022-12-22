package learning.cats.rvjexercises

import cats.Functor
import cats.syntax.functor._

object CatsExFunctor extends App {
  // define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]

  object Tree {
    def leaf[T](value: T): Leaf[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Branch[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(value) => Tree.leaf(f(value))
        case Branch(value, l, r) => Tree.branch(f(value), map(l)(f), map(r)(f))
      }
    }
  }

  val testTree =
    Tree.branch(
      10,
      Tree.branch(
        30,
        Tree.leaf(40),
        Tree.leaf(25)
      ),
      Tree.leaf(60)
    )

  def double[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 2)

  println(double[Tree](testTree))

  //shorter double method
  def doubleShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 2)

  println(doubleShorter[Tree](testTree))

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => self.format(func(value))
  }
}
