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
}
