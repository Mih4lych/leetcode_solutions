package learning.ziorvj

import learning.ziorvj.utils._
import zio._

object ZIOEx12 extends ZIOAppDefault {
  /**
   * Exercise
   * 1. what is the code SUPPOSED to do?
   * 2. find if there's anything wrong
   * 3. fix the problem
   */

  val mySemaphore = Semaphore.make(1) // a mutex
  val tasks = ZIO.collectAllPar((1 to 10).map { id =>
    for {
      sem <- mySemaphore
      _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
      res <- sem.withPermit {
        for {
          // critical section start
          _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWorkWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
        } yield res
      }
    } yield res
  })
  val taskFixed = mySemaphore.flatMap { sem =>
    ZIO.collectAllPar((1 to 10).map { id =>
      for {
        _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
        res <- sem.withPermit {
          for {
            // critical section start
            _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
            res <- doWorkWhileLoggedIn()
            _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
          } yield res
        }
      } yield res
    })
  }

  // example: limiting the number of concurrent sessions on a server
  def doWorkWhileLoggedIn(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def run = taskFixed
}
