package learning.cats.rvjexercises

import cats.Monoid
import cats.instances.int._
import cats.instances.map._
import cats.instances.string._
import cats.syntax.monoid._

object CatsExMonoid extends App {
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  println(combineFold(List("asdasd", "asdasdasd", " asdasdasd")))
  println(combineFold(List(1, 3, 4, 2)))

  // combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid - use an import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )
  println(combineFold(phonebooks))

  // shopping cart and online stores with Monoids
  // hint: define your monoid - Monoid.instance
  // hint #2: use combineByFold
  case class ShoppingCard(items: List[String], total: Double)

  implicit val shoppingCard: Monoid[ShoppingCard] = Monoid.instance[ShoppingCard](ShoppingCard(List.empty, 0D), (shoppingCard1, shoppingCard2) => ShoppingCard(shoppingCard1.items |+| shoppingCard2.items, shoppingCard1.total |+| shoppingCard2.total))

  def checkout(shoppingCards: List[ShoppingCard]): ShoppingCard = combineFold(shoppingCards)

  println(checkout(List(ShoppingCard(List("asd"), 123), ShoppingCard(List("asdasd", "asdasd"), 30000))))
}
