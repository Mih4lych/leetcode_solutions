package learning.basics

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](value: B): MyList[B]
  def printElements: String
  override def toString: String = s"[$printElements]"
  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def ++[B >: A](otherList: MyList[B]): MyList[B]
}

case object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException("list is empty")
  def tail: MyList[Nothing] = throw new NoSuchElementException("list is empty")
  def isEmpty: Boolean = true
  def add[B](value: B): MyList[B] = Cons(value, this)
  def printElements: String = ""
  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = this
  def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = this
  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = this
  def ++[B >: Nothing](otherList: MyList[B]): MyList[B] = otherList
}

final case class Cons[A](private val h: A, private val t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](value: B): MyList[B] = Cons(value, this)
  def printElements: String =
    if (t.isEmpty) h.toString
    else s"$h, ${t.printElements}"
  def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    Cons(transformer.transform(head), t.map(transformer))
  }
  def ++[B >: A](otherList: MyList[B]): MyList[B] = Cons(h, t ++ otherList)
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)
  def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) Cons(h, t.filter(predicate)) else t.filter(predicate)
  }
}

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
}