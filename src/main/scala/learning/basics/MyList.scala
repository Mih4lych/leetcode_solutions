package learning.basics

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](value: B): MyList[B]
  def printElements: String
  override def toString: String = s"[$printElements]"
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def ++[B >: A](otherList: MyList[B]): MyList[B]
}

case object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException("list is empty")
  def tail: MyList[Nothing] = throw new NoSuchElementException("list is empty")
  def isEmpty: Boolean = true
  def add[B](value: B): MyList[B] = Cons(value, this)
  def printElements: String = ""
  def map[B](transformer: Nothing => B): MyList[B] = this
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = this
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = this
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
  def map[B](transformer: A => B): MyList[B] = {
    Cons(transformer(head), t.map(transformer))
  }
  def ++[B >: A](otherList: MyList[B]): MyList[B] = Cons(h, t ++ otherList)
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
  def filter(predicate: A => Boolean): MyList[A] = {
    if (predicate(h)) Cons(h, t.filter(predicate)) else t.filter(predicate)
  }
}