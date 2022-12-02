package learning.fppractice

import scala.annotation.tailrec

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
}
