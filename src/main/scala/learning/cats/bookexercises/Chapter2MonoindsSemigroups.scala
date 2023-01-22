package learning.cats.bookexercises

object Chapter2MonoindsSemigroups extends App {
  object MonoidRealisation {
    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    object Monoid {
      def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
    }

    implicit def setMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        def empty: Set[A] = Set.empty[A]

        def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
      }
  }

  import cats.Monoid
  import cats.instances.int._
  import cats.syntax.monoid._

  def add(list: List[Int]): Int = list.foldLeft(0)(_ |+| _)

  def addUniversal[A : Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = Monoid.instance[Order](
    Order(0, 0),
    (ord1, ord2) => Order(ord1.totalCost + ord2.totalCost, ord1.quantity + ord2.quantity)
  )
}
