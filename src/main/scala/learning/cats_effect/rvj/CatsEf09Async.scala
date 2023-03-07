package learning.cats_effect.rvj

import cats.effect.{IO, IOApp}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object CatsEf09Async extends IOApp.Simple {
  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread...")
    42
  }

  def asyncToIO[A](computation: () => A)(implicit ec: ExecutionContext): IO[A] = {
    IO.async_ { cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither

        cb(result)
      }
    }
  }

  lazy val molFuture: Future[Int] = Future { computeMeaningOfLife() }

  def futureToIO[A](future: => Future[A]): IO[A] = {
    IO.async_ { cb =>
      future.onComplete(res => cb(res.toEither))
    }
  }

  def neverEnding: IO[Nothing] = {
    IO.async_ { cb =>
      ()
    }
  }


  override def run: IO[Unit] = ???
}
