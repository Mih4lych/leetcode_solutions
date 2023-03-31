package learning.fs2.udemy

import cats.effect.IO
import fs2._

import scala.concurrent.duration.FiniteDuration

class Task04 {
  val data = List.range(1, 100)
  val pageSize = 20

  def fetchPage(pageNumber: Int): IO[List[Int]] = {
    val start = pageNumber * pageSize
    val end = start + pageSize

    IO.println(s"Fetch the page: $pageNumber").as(data.slice(start, end))
  }

  def fetchAll(): Stream[IO, Int] = {
    Stream.unfoldEval(0) { pageNumber =>
      fetchPage(pageNumber).map { data =>
        if (data.isEmpty) None
        else Some(Stream.emits(data), pageNumber + 1)
      }
    }.flatten
  }

  def evalEvery[A](t: FiniteDuration)(ef: IO[A]): Stream[IO, A] = {
    Stream.sleep_[IO](t) ++ Stream.eval(ef) ++ evalEvery(t)(ef)
  }

  implicit class ReachStream[A](s: Stream[IO, A]) {
    def flatAttempt: Stream[IO, A] = {
      s
        .attempt
        .collect {
          case Right(value) => value
        }
    }
  }
}
