package learning.cats.rvjexercises

import cats.Eval

object CatsExEvaluation extends App{
  //implement defer
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  val eval = defer(Eval.now {
    println("asd")
    42
  })

  //rewrite using eval
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(List())
    else Eval.defer(reverseEval(list.tail).map(newList => newList :+ list.head))
  }

  println(reverseEval((1 to 1000).toList).value)
}
