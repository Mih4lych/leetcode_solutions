package learning.cats.rvjexercises

import cats.data.State

object CatsExState extends App {
  //an online store

  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, price + cart.total), cart.total + price)
  }

  // pure mental gymnastics
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(a => (a, a))

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))
}
