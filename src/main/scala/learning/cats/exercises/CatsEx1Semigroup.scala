package learning.cats.exercises

import cats.instances.int._
import cats.instances.double._
import cats.syntax.semigroup._
import cats.Semigroup

object CatsEx1Semigroup {
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {(ex1, ex2) =>
    Expense(ex1.id.max(ex2.id), Semigroup[Double].combine(ex1.amount, ex2.amount))
  }

  def reduce[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)
  def reduce2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(reduce(List(Expense(1, 3), Expense(3, 10))))
    println(reduce2(List(Expense(1, 3), Expense(3, 10))))
    println(10 |+| 10)
  }
}
