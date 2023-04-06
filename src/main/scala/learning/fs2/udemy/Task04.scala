package learning.fs2.udemy

import cats.effect.IO
import fs2._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

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

  def doEffectFailing[A](ef: IO[A]): IO[A] = {
    IO(math.random()).flatMap { flag =>
      if (flag < 0.5) IO.println("Failing") *> IO.raiseError(new Exception("boom"))
      else IO.println("Successful") *> ef
    }
  }

  val searches = Stream.iterateEval("")(s => IO(Random.nextPrintableChar()).map(s + _))
  def performSearch(text: String): IO[Unit] = doEffectFailing(IO.println(s"Try to find text: $text"))

  def performSearchRetrying(text: String): Stream[IO, Unit] =
    Stream.retry(
      performSearch(text),
      1.second,
      _ + 1.second,
      5
    )

  searches
    .metered(200.millis)
    .debounce(500.millis)
    .flatMap(performSearchRetrying)
    .interruptAfter(5.second)
    .compile


}
