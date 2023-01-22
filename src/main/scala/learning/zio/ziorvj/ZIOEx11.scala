package learning.zio.ziorvj

import zio._

import utils._

object ZIOEx11 extends ZIOAppDefault {

  /**
   * Exercises
   * 1. Write a simulated "egg boiler" with two ZIOs
   *  - one increments a counter every 1s
   *  - one waits for the counter to become 10, after which it will "ring a bell"
   *
   * 2. Write a "race pair"
   *  - use a Promise which can hold an Either[exit for A, exit for B]
   *  - start a fiber for each ZIO
   *  - on completion (with any status), each ZIO needs to complete that Promise
   *    (hint: use a finalizer)
   *  - waiting on the Promise's value can be interrupted!
   *  - if the whole race is interrupted, interrupt the running fibers
   */
  def eggBoiler(): Task[Unit] = {
    def counter(count: Ref[Int], promise: Promise[Throwable, Int], countToStop: Int): Task[Unit] = {
      for {
        _ <- ZIO.succeed("waiting next 1 second").debugThread
        _ <- ZIO.sleep(1.seconds)
        curCount <- count.updateAndGet(_ + 1)
        stop <- ZIO.succeed(countToStop)
        _ <-
          if (curCount == stop) promise.succeed(curCount)
          else counter(count, promise, countToStop)
      } yield ()
    }

    def ring(promise: Promise[Throwable, Int]): Task[Unit] = {
      for {
        _ <- ZIO.succeed("waiting for timer").debugThread
        time <- promise.await
        _ <- ZIO.succeed(s"waiting completed: $time").debugThread
      } yield ()
    }

    for {
      ref <- Ref.make(0)
      promise <- Promise.make[Throwable, Int]
      _ <- ZIO.succeed("start to boil eggs")
      _ <- counter(ref, promise, 10) zipPar ring(promise)
    } yield ()
  }

  //2
  def racePair[R, E, A, B](zioa: => ZIO[R, E, A], ziob: ZIO[R, E, B]):
    ZIO[R, Nothing, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] = {
    ZIO.interruptibleMask { restore =>
      for {
        promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
        fibera <- zioa.onExit(exit => promise.succeed(Left(exit))).fork
        fiberb <- ziob.onExit(exit => promise.succeed(Right(exit))).fork
        result <- restore(promise.await).onInterrupt {
          for {
            inta <- fibera.interrupt.fork
            intb <- fiberb.interrupt.fork
            _ <- inta.join
            _ <- intb.join
          } yield ()
        }
      } yield result match {
        case Left(exit) => Left(exit, fiberb)
        case Right(exit) => Right(fibera, exit)
      }
    }
  }

  def run = eggBoiler()
}
