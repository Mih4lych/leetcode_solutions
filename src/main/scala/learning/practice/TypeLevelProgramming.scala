package learning.practice

object TypeLevelProgramming extends App {
  trait Nat
  class _0 extends Nat
  class Succ[N <: Nat] extends Nat

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]

  trait <[A <: Nat, B <: Nat]
  object < {
    implicit def ltBasic[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}
    implicit def inductive[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[Succ[A], Succ[B]] = new <[Succ[A], Succ[B]] {}
    def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[A, B] = lt
  }

  val test = <[_3, _4]

  trait <=[A <: Nat, B <: Nat]

  object <= {
    implicit def lteBasic[B <: Nat]: <=[_0, B] = new <=[_0, B] {}

    implicit def inductive[A <: Nat, B <: Nat](implicit lte: <=[A, B]): <=[Succ[A], Succ[B]] = new <=[Succ[A], Succ[B]] {}

    def apply[A <: Nat, B <: Nat](implicit lte: <=[A, B]): <=[A, B] = lte
  }

  val test2 = <=[_1, _1]


  trait >=[A <: Nat, B <: Nat]

  object >= {
    implicit def gteBasic[B <: Nat]: >=[B, _0] = new >=[B, _0] {}

    implicit def inductive[A <: Nat, B <: Nat](implicit gte: >=[A, B]): >=[Succ[A], Succ[B]] = new >=[Succ[A], Succ[B]] {}

    def apply[A <: Nat, B <: Nat](implicit gte: >=[A, B]): >=[A, B] = gte
  }
  trait >[A <: Nat, B <: Nat]

  object > {
    implicit def gtBasic[B <: Nat]: >[Succ[B], _0] = new >[Succ[B], _0] {}

    implicit def inductive[A <: Nat, B <: Nat](implicit gt: >[A, B]): >[Succ[A], Succ[B]] = new >[Succ[A], Succ[B]] {}

    def apply[A <: Nat, B <: Nat](implicit gt: >[A, B]): >[A, B] = gt
  }

  val test3 = >=[_3, _3]
  val test4 = >[_4, _3]
}
