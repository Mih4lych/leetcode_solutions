package learning.cats_effect.rvj

import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore
import cats.syntax.parallel._
import learning.cats_effect.utils.DebugWrapper

import scala.concurrent.duration._
import scala.util.Random

object CatsEf13Semaphore extends IOApp.Simple {
  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))


  val users: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting for log in").debug
        _ <- sem.acquire
        _ <- IO(s"[session $id] logged in, working").debug
        res <- doWorkWhileLoggedIn()
        _ <- IO(s"[session $id] success").debug
        _ <- sem.release
      } yield res
    }
  }

  override def run: IO[Unit] = users.debug.void
}
