package learning.fppractice

import scala.annotation.tailrec
import scala.collection.immutable._

object StringProblems extends App{
  def countCharacters(string: String): Map[Char, Int] = {
    @tailrec
    def rec(cars: Seq[Char], acc: Map[Char, Int]): Map[Char, Int] = {
      cars match {
        case h +: t =>
          val curCount = acc.getOrElse(h, 0)

          rec(t, acc + (h -> (curCount + 1)))
        case _ =>
          acc
      }
    }

    rec(string.toSeq, Map.empty)
  }

  println(countCharacters("ddddaaaassdsaav"))

  def checkAnagrams(sa: String, sb: String): Boolean = {
    countCharacters(sa).equals(countCharacters(sb))
  }

  def checkAnagrams2(sa: String, sb: String): Boolean = {
    sa.sorted == sb.sorted
  }

  println(checkAnagrams("scala", "alacs"))
  println(checkAnagrams("scala", "asda"))
  println(checkAnagrams("scala", ""))

  println(checkAnagrams2("scala", "alacs"))
  println(checkAnagrams2("scala", "asda"))
  println(checkAnagrams2("scala", ""))

  def hasValidParentheses(string: String): Boolean = {
    @tailrec
    def rec(chars: Seq[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else {
        val nextCheck = chars.head
        val charFromStack = stack.headOption

        if (nextCheck == '(') {
          charFromStack match {
            case Some(char) =>
              if (char == nextCheck) rec(chars.tail, nextCheck :: stack)
              else rec(chars.tail, stack.tail)
            case None =>
              rec(chars.tail, nextCheck :: stack)
          }
        }
        else {
          charFromStack match {
            case Some(_) =>
              rec(chars.tail, stack.tail)
            case None =>
              false
          }
        }
      }
    }

    rec(string.toSeq, List.empty)
  }

  println(hasValidParentheses("()()()"))

  def generateAllValidParentheses(n: Int): List[String] = {
    def loop(rem: Int, set: Set[String]): List[String] = {
      if (rem == 0) set.toList
      else {
        loop(rem - 1, set.flatMap(s => Set(s"()$s", s"($s)", s"$s()")))
      }
    }

    loop(n, Set(""))
  }

  println(generateAllValidParentheses(3))

  def ransomNote(note: String, magazine: String): Boolean = {
    val magazineCount = countCharacters(magazine.replaceAll(" ", ""))

    countCharacters(note.replaceAll(" ", "")).forall{case (char, count) =>
      magazineCount.contains(char) && magazineCount(char) >= count
    }

  }

  def compareVersionNumbers(version1: String, version2: String): Int = {
    def rec(remVer1: List[Int], remVer2: List[Int]): Int = {
      (remVer1, remVer2) match {
        case (Nil, Nil) => 0
        case (v1, Nil) =>
          if (v1.exists(_ != 0)) 1
          else 0
        case (Nil, v2) =>
          if (v2.exists(_ != 0)) -1
          else 0
        case (v1h :: t1, v2h :: t2) =>
          if (v1h > v2h) 1
          else if (v1h < v2h) -1
          else rec(t1, t2)
      }
    }

    def getProperIntRep(verPart: String): Int = {
      verPart.toInt
    }

    rec(version1.split("\\.").map(getProperIntRep).toList, version2.split("\\.").map(getProperIntRep).toList)
  }

  println(compareVersionNumbers("1.0.3.4", "1.0.03.4"))

  def multiplyStrings(a: String, b: String): String = {
    def rec(curResult: String, indexB: Int): String = {
      if (indexB < 0) curResult
      else {
        val intForMultiply = b(indexB) - '0'
        val multipliedA = if (intForMultiply == 0) "0"
          else {
            val (res, rem) = a.reverse.foldLeft(("", 0)){(acc, char) =>
              val (str, remInt) = acc
              val result = (char - '0') * intForMultiply + remInt

              if (result < 10) (result.toString + str, 0)
              else {
                val (nextRem, strForAdd) = result.toString.splitAt(1)

                (strForAdd + str, nextRem.head - '0')
              }
            }

            if (rem > 0) rem + res
            else res
          }
        val middlePart =
          if (curResult.isEmpty) (multipliedA.tail, 0)
          else {
            curResult.init.zip(multipliedA.tail).reverse.foldLeft(("", 0)) { (acc, chars) =>
              val (str, remInt) = acc
              val result = chars._1 + chars._2 - ('0' * 2) + remInt

              if (result < 10) (result.toString + str, 0)
              else {
                val (nextRem, strForAdd) = result.toString.splitAt(1)

                (strForAdd + str, nextRem.head - '0')
              }
            }
          }

        val head =
          if (middlePart._2 > 0) (multipliedA.head - '0' + middlePart._2).toString else multipliedA.head.toString
        rec(head + middlePart._1 + curResult.lastOption.getOrElse(""), indexB - 1)
      }
    }

    rec("", b.length - 1)
  }

  println(multiplyStrings("10", "100"))

  def reorganizeString(str: String): String = ???

  def revertOrder(str: String): String = {
    str.trim.split(" ").filter(_.nonEmpty).reverse.mkString(" ")
  }
}
