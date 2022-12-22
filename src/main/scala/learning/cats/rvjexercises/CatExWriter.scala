package learning.cats.rvjexercises

import cats.data.Writer
import cats.instances.vector._

import scala.annotation.tailrec

object CatExWriter extends App {
  //rewrite it
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), n)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(n.toString), n))
  }

  def counterAndLogTailRec(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def rec(curInt: Int, curWriter: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (curInt > n) curWriter
      else rec(curInt + 1, curWriter.flatMap(_ => Writer(Vector(curInt.toString), curInt)))
    }

    rec(1, Writer(Vector("starting"), 0))
  }

  countAndLog(10).written.foreach(println)
  counterAndLogTailRec(10).written.foreach(println)

  // rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def rec(curInt: Int, curWriter: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (curInt > n) curWriter
      else {
        val newWriter = for {
          acc <- curWriter
          wrBeforeCalc <- Writer(Vector(s"Now at $curInt"), curInt)
          newSum = acc + wrBeforeCalc
          wrAfterCalc <- Writer(Vector(s"Computed sum($curInt) = $newSum"), newSum)
        } yield wrAfterCalc

        rec(curInt + 1, newWriter)
      }
    }

    if (n <= 0) Writer(Vector(), 0)
    else rec(1, Writer(Vector(), 0))
  }

  sumWithLogs(100).written.foreach(println)
}
