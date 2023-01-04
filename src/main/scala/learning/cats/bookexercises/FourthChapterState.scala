package learning.cats.bookexercises

import cats.data.State
import cats.syntax.applicative._

import scala.util.{Failure, Success, Try}

object FourthChapterState extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "/" => operand(_ / _)
      case "*" => operand(_ * _)
      case "-" => operand(_ - _)
      case "+" => operand(_ + _)
      case num => operator(num.toIntOption)
    }

  def operator(num: Option[Int]): CalcState[Int] = State { list =>
    num match {
      case Some(value) => (value :: list, value)
      case None => throw new NumberFormatException("Not a correct number format")
    }
  }

  def operand(f: (Int, Int) => Int): CalcState[Int] = State {
    case second :: first :: t =>
      val result = f(first, second)
      (result :: t, result)
    case _ => throw new NullPointerException("Stack is empty")
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((a, b) => a.flatMap(_ => evalOne(b)))

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  println(evalInput("1 2 + 3 4 + *"))

  val testFor =
    for {
      _ <- evalAll(List("2", "3", "*"))
      _ <- evalAll(List("5", "5", "*"))
      res <- evalOne("+")
    } yield res

  println(testFor.runA(Nil).value)
}
