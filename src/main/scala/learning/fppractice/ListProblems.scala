package learning.fppractice

import zio.test.{Result, live}

import java.util.Random

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[R >: T](elem: R): RList[R] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[R >: T](otherList: RList[R]): RList[R]
  def removeAt(index: Int): RList[T]
  def map[R](f: T => R): RList[R]
  def flatMap[R](f: T => RList[R]): RList[R]
  def filter(f: T => Boolean): RList[T]
  def rle: RList[(T, Int)]
  def duplicateEach(time: Int): RList[T]
  def rotate(steps: Int): RList[T]
  def sample(size: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = this
  override def ++[R >: Nothing](otherList: RList[R]): RList[R] = otherList
  override def removeAt(index: Int): RList[Nothing] = this

  override def map[R](f: Nothing => R): RList[R] = this

  override def flatMap[R](f: Nothing => RList[R]): RList[R] = this

  override def filter(f: Nothing => Boolean): RList[Nothing] = this

  override def rle: RList[(Nothing, Int)] = this

  override def duplicateEach(time: Int): RList[Nothing] = this
  override def rotate(steps: Int): RList[Nothing] = this
  override def sample(size: Int): RList[Nothing] = this

  override def toString: String = "[]"
}

case class ::[T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  def applyNaive(index: Int): T = {
    if (index < 0) throw new IndexOutOfBoundsException
    else if (index == 0) head
    else tail.apply(index - 1)
  }
  override def length: Int = {
    def lengthRec(list: RList[T], curLength: Int): Int = {
      list match {
        case _ :: t => lengthRec(t, curLength + 1)
        case RNil => curLength
      }
    }

    lengthRec(this, 0)
  }

  override def reverse: RList[T] = {
    def reverseRec(curReversedList: RList[T], curReversibleList: RList[T]): RList[T] = {
      curReversibleList match {
        case h :: tail => reverseRec(h :: curReversedList, tail)
        case RNil => curReversedList
      }
    }

    reverseRec(RNil, this)
  }

  override def apply(index: Int): T = {
    def applyRec(list: RList[T], curIndex: Int): T = {
      if (curIndex == 0) list.head
      else {
        list match {
          case _ :: t => applyRec(t, curIndex - 1)
          case RNil => throw new IndexOutOfBoundsException
        }
      }
    }

    if (index < 0) throw new IndexOutOfBoundsException
    else applyRec(this, index)
  }

  override def ++[R >: T](otherList: RList[R]): RList[R] = {
    def concatRec(remList: RList[R], result: RList[R]): RList[R] = {
      remList match {
        case h :: t => concatRec(t, h:: result)
        case RNil => result
      }
    }

    concatRec(this.reverse, otherList)
  }

  override def removeAt(index: Int): RList[T] = {
    def removeAtRec(rem: RList[T], result: RList[T], counter: Int): RList[T] = {
      if (counter == index) {
        rem match {
          case _ :: t => result.reverse ++ t
          case RNil => this
        }
      }
      else {
        rem match {
          case h :: t => removeAtRec(t, h :: result, counter + 1)
          case RNil => this
        }
      }
    }

    if (index < 0) this
    else removeAtRec(this, RNil, 0)
  }

  override def map[R](f: T => R): RList[R] = {
    def mapRec(curList: RList[T], newList: RList[R]): RList[R] = {
      curList match {
        case h :: t => mapRec(t, f(h) :: newList)
        case RNil => newList.reverse
      }
    }

    mapRec(this, RNil)
  }

  override def flatMap[R](f: T => RList[R]): RList[R] = {
    def flatMapRec(curList: RList[T], newList: RList[R]): RList[R] = {
      curList match {
        case h :: t => flatMapRec(t, f(h).reverse ++ newList)
        case RNil => newList.reverse
      }
    }

    def betterFlatMap(curList: RList[T], acc: RList[RList[R]]): RList[R] = {
      curList match {
        case h :: t => betterFlatMap(t, f(h).reverse :: acc)
        case RNil => concatAll(acc, RNil, RNil)
      }

    }

    def concatAll( remLists: RList[RList[R]], curList: RList[R], acc: RList[R]): RList[R] = {
      curList match {
        case h :: t => concatAll(remLists, t, h :: acc)
        case RNil =>
          remLists match {
            case h :: t => concatAll(t, h, acc)
            case RNil => acc
          }
      }
    }

    betterFlatMap(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    def filterRec(curList: RList[T], newList: RList[T]): RList[T] = {
      curList match {
        case h :: t => filterRec(t, if (f(h)) h :: newList else newList)
        case RNil => newList.reverse
      }
    }

    filterRec(this, RNil)
  }

  override def rle: RList[(T, Int)] = {
    def rleRec(curList: RList[T], acc: RList[(T, Int)], curPair: (T, Int)): RList[(T, Int)] = {
      curList match {
        case h :: t =>
          if (curPair._1 == h)
            rleRec(t, acc, (curPair._1, curPair._2 + 1))
          else {
            rleRec(t, curPair :: acc, (h, 1))
          }
        case RNil => (curPair :: acc).reverse
      }
    }

    rleRec(this.tail, RNil, (this.head, 1))
  }

  override def duplicateEach(time: Int): RList[T] = {
    def duplicateEachRec(curList: RList[T], result: RList[T]): RList[T] = {
      curList match {
        case h :: t => duplicateEachRec(t, (1 to time).foldLeft(result)((acc, _) => h :: acc))
        case RNil => result.reverse
      }
    }

    duplicateEachRec(this, RNil)
  }

  override def rotate(steps: Int): RList[T] = {
    def rotateRec(rem: RList[T], shift: RList[T], remSteps: Int): RList[T] = {
      if (remSteps == 0) {
        rem ++ shift.reverse
      }
      else {
        rem match {
          case h :: t => rotateRec(t, h :: shift, remSteps - 1)
          case RNil => rotateRec(this.tail, this.head :: RNil, remSteps - 1)
        }
      }
    }

    rotateRec(this, RNil, steps)
  }

  override def sample(size: Int): RList[T] = {
    val length = this.length

    def sampleRec(result: RList[T], remSize: Int): RList[T] = {
      if (remSize == 0) result.reverse
      else {
        sampleRec(this(new Random().nextInt(length)) :: result, remSize - 1)
      }
    }
    def sampleElegant: RList[T] =
      (1 to size).foldLeft(RNil.asInstanceOf[RList[T]])((acc, _) => this(new Random().nextInt(length)) :: acc)

    if (size < 0) RNil
    else sampleElegant
  }


  override def toString: String = {
    def toStringRec(list: RList[T], accStr: String): String = {
      list match {
        case ::(h, t) => toStringRec(t, s"$accStr, $h")
        case RNil => accStr
      }
    }

    s"[${toStringRec(this, "")}]"
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def fromRec(rem: Iterable[T], result: RList[T]): RList[T] = {
      if (iterable.nonEmpty) {
        fromRec(iterable.tail, iterable.head :: result)
      }
      else {
        result.reverse
      }
    }

    fromRec(iterable, RNil)
  }

  def duplicateEach_v2[T](list: RList[T], time: Int): RList[T] = {
    def duplicateEachRec(curList: RList[T], dupRem: Int, result: RList[T]): RList[T] = {
      curList match {
        case h :: _ if dupRem > 0 => duplicateEachRec(curList, dupRem - 1, h :: result)
        case _ :: t if dupRem == 0 => duplicateEachRec(t, time, result)
        case RNil => result.reverse
      }
    }

    duplicateEachRec(list, time, RNil)
  }
}

object ListProblems extends App {
  val testReverse = (1 :: 2 :: 3 :: RNil).reverse
  val testRLE = (1 :: 1 :: 2 :: 3 :: 3 :: 3 :: 4 :: RNil).reverse

  println(testReverse)
  println(testReverse.removeAt(0))
  println(testReverse.flatMap(1 :: _ :: RNil))
  println(testRLE.rle)
  println(testReverse.duplicateEach(2))
  println(RList.duplicateEach_v2(testReverse, 2))
  println(testReverse.rotate(2))
  println(testReverse.rotate(3))
  println(testReverse.rotate(6))
  println(testReverse.sample(6))
  println(testReverse.flatMap(1 :: _ :: RNil))
}
