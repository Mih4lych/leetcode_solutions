package learning.fppractice

import scala.annotation.tailrec
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
    if (number == Int.MinValue) 0
    else {
      val result = number.abs.toString.reverse.toInt * number.sign

      if (result.sign != number.sign) 0
      else result
    }
  }

  println(reverseInt(-54))
}
