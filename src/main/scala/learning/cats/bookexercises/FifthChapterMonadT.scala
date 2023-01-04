package learning.cats.bookexercises

import cats.data.EitherT
import cats.syntax.applicative._
import cats.syntax.applicativeError._

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object FifthChapterMonadT extends App {
  type Response[A] = EitherT[Future, String, A]

  implicit val context: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None => EitherT.left(Future(s"$autobot not in the crew"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield powerLevel1 + powerLevel2 > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(massage) => s"Comms error: $massage"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }

  println(tacticalReport("Jazz", "Bumblebee"))
  // res13: String = "Jazz and Bumblebee need a recharge."
  println(tacticalReport("Bumblebee", "Hot Rod"))
  // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
  println(tacticalReport("Jazz", "Ironhide"))
  // res15: String = "Comms error: Ironhide unreachable"
}
