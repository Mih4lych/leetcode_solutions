package learning.fppractice

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.util.Random

object NumberProblems extends App {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeRec(curDivider: Int): Boolean = {
      if (curDivider > Math.sqrt(Math.abs(n))) true
      else {
        n % curDivider == 0 && isPrimeRec(curDivider + 1)
      }
    }

    if (n == -1 || n == 0 || n == 1) false
    else isPrimeRec(2)
  }

  def decompose(n: Int): List[Int] = {
    assert(n >= 0)
    @tailrec
    def decomposeRec(rem: Int, curDivider: Int, acc: List[Int]): List[Int] = {
      if (curDivider > Math.sqrt(Math.abs(n))) rem :: acc
      else {
        if (rem % curDivider == 0) decomposeRec(rem / curDivider, curDivider, curDivider :: acc)
        else decomposeRec(rem, curDivider + 1, acc)
      }
    }

    decomposeRec(n, 2, List.empty)
  }

  def approximatePi(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())
    def rec(curPointNum: Int, pointsInCircle: Int): Double = {
      if (curPointNum > nPoints) pointsInCircle * 4.0 / nPoints
      else {
        val x = random.nextDouble()
        val y = random.nextDouble()

        if (x * x + y * y < 1) {
          rec(curPointNum + 1, pointsInCircle + 1)
        }
        else {
          rec(curPointNum + 1, pointsInCircle)
        }
      }
    }

    rec(0, 0)
  }

  println(approximatePi(10000000))

  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = {
    def rec(rem: Int, remMap: Map[Int, Int], curPos: Int, acc: String): String = {
      if (rem % denominator == 0) {
        acc + (rem / denominator)
      }
      else if(remMap.contains(rem)) {
        val (notRecPart, recPart) = acc.splitAt(remMap(rem))
        s"$notRecPart($recPart)"
      } else {
        val newMap = remMap + (rem -> curPos)

        rec(rem % denominator * 10, newMap, curPos + 1, acc + (rem / denominator))
      }
    }

    s"${numerator / denominator}${if (numerator % denominator != 0) s".${rec(numerator % denominator * 10, Map.empty, 0, "")}" else ""}"
  }

  println(fractionToRecurringDecimals(1, 2))

  def largestNumber(nums: List[Int]): String = {
    implicit val sort = Ordering.fromLessThan[Int]{(a, b) =>
      s"$a$b" >= s"$b$a"
    }

    val result = nums.sorted.mkString

    if (result.startsWith("0")) "0"
    else result
  }

  println(largestNumber(List(3, 30, 5, 9, 34)))

  def reverseInt(number: Int): Int = {
    def rec(rem: Int, acc: Int): Int = {
      if (rem == 0) acc
      else {
        val newAcc = acc * 10 + rem % 10

        if (newAcc.sign < 0) 0
        else rec(rem / 10, newAcc)
      }
    }

    if (number == Int.MinValue) 0
    else if (number < 0) -rec(-number, 0)
    else rec(number, 0)
  }

  println(reverseInt(-54))
  println(reverseInt(Int.MaxValue))

  def parseInteger(str: String): Int = {
    def rec(curStr: String, acc: Int, sign: Int): Int = {
      if (curStr.isEmpty || !curStr.head.isDigit) acc * sign
      else {
        val newAcc = acc * 10 + curStr.head.asDigit

        if (newAcc < 0) {
          if (sign < 0) Int.MinValue
          else Int.MaxValue
        }
        else {
          rec(curStr.tail, newAcc, sign)
        }
      }
    }

    val trimStr = str.trim

    val (sign, strWithoutSign) =
      trimStr.headOption match {
        case Some('-') => (-1, trimStr.tail)
        case Some('+') => (1, trimStr.tail)
        case _ => (1, trimStr)
      }

    rec(strWithoutSign, 0, sign)
  }

  println(parseInteger("3912993002391010839293089"))
  println(parseInteger("-3912993002391010839293089"))
  println(parseInteger(""))
  println(parseInteger("  342asda "))
  println(parseInteger("  0000231  "))

  def uglyNumber(number: Int): Boolean = {
    def rec(rem: Int, dividers: List[Int]): Boolean = {
      if (rem == 1) true
      else if (dividers.isEmpty) false
      else {
        if (rem % dividers.head == 0) {
          rec(rem / dividers.head, dividers)
        }
        else {
          rec(rem, dividers.tail)
        }
      }
    }

    if (number <= 0) false
    else rec(number, List(2, 3, 5))
  }

  println(uglyNumber(6))
  println(uglyNumber(15))
  println(uglyNumber(100))
  println(uglyNumber(1))

  println(uglyNumber(14))
  println(uglyNumber(39))


  def nthUgly(index: Int): Int = {
    def rec(leftSteps: Int, curUgly: Int, next: Int): Int = {
      if (leftSteps == 0) curUgly
      else {
        if (next % 2 == 0) rec(leftSteps - 1, next, next + 1)
        else if (next % 3 == 0) rec(leftSteps - 1, next, next + 1)
        else if (next % 5 == 0) rec(leftSteps - 1, next, next + 1)
        else rec(leftSteps, curUgly, next + 1)
      }
    }

    def recWithQueues(leftSteps: Int, curUgly: Int, queueFor2: Queue[Int], queueFor3: Queue[Int], queueFor5: Queue[Int]): Int = {
      if (leftSteps == 0) curUgly
      else {
        val (ugly2, left2) = queueFor2.dequeue
        val (ugly3, left3) = queueFor3.dequeue
        val (ugly5, left5) = queueFor5.dequeue
        val minUgly = ugly2.min(ugly3.min(ugly5))

        val newQueueFor2 = (if (ugly2 == minUgly) left2 else queueFor2).enqueue(minUgly * 2)
        val newQueueFor3 = (if (ugly3 == minUgly) left3 else queueFor3).enqueue(minUgly * 3)
        val newQueueFor5 = (if (ugly5 == minUgly) left5 else queueFor5).enqueue(minUgly * 5)

        recWithQueues(leftSteps - 1, minUgly, newQueueFor2, newQueueFor3, newQueueFor5)
      }
    }

    //rec(index, 1, 1)
    if (index == 0) 1
    else recWithQueues(index, 1, Queue(2), Queue(3), Queue(5))
  }

  println((0 to 100).map(nthUgly).toList)


  def duplicatesBySorted(list: List[Int]): Int = {
    def rec(sortedList: List[Int]): Int = {
      sortedList match {
        case x :: y :: t =>
          if (x == y)
            rec(t)
          else
            x
        case x :: Nil => x
      }
    }

    rec(list.sorted)
  }

  def duplicatesBySet(list: List[Int]): Int = {
    def rec(curList: List[Int], set: Set[Int]): Int = {
      if (curList.isEmpty) set.head
      else {
        if (set.contains(curList.head)) rec(curList.tail, set - curList.head)
        else  rec(curList.tail, set + curList.head)
      }
    }

    rec(list, Set.empty)
  }

  println(duplicatesBySorted(List(1, 4, 3, 4, 5, 1, 3)))
  println(duplicatesBySet(List(1, 4, 3, 4, 5, 1, 3)))
  println(List(1, 4, 3, 4, 5, 1, 3).foldLeft(0)(_ ^ _))
}
