package learning.ziorvj

import zio._
import scala.collection.immutable.Queue
import utils._

abstract class Mutex {
  def acquire: UIO[Unit]

  def release: UIO[Unit]
}

object Mutex {
  type Signal = Promise[Nothing, Unit]

  case class State(locked: Boolean = false, waiting: Queue[Signal] = Queue.empty)

  val innerState = State()

  def make: UIO[Mutex] = Ref.make(innerState).map { state =>
    new Mutex {

      /*
        Change the state of the Ref
        - if the mutex is unlocked, lock it
        - if the mutex is locked, state becomes (true, queue + new signal) and WAIT on that signal
       */
      override def acquire: UIO[Unit] = ???

      /*
              Change the state of the Ref
              - if the mutex is unlocked, leave the state unchanged
              - if the mutex is locked
                - if the queue is empty, unlock the mutex
                - if the queue is non-empty, take a signal out of the queue and complete it
             */
      override def release: UIO[Unit] = ???
    }
  }
}

object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks() =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[task ${i}] working...").debugThread
        result <- workInCriticalRegion()
        _ <- ZIO.succeed(s"[task ${i}] got result: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _ <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
      _ <- mutex.acquire
      // critical region start
      _ <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion().onInterrupt(mutex.release)
      _ <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
      // critical region end
      _ <- mutex.release
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] was interrupted.").debugThread)
      .onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause"))
  }

  def demoLockingTasks() = for {
    mutex <- Mutex.make
    _ <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0)
      createTask(id, mutex)
    else for {
      fib <- createTask(id, mutex).fork
      _ <- ZIO.sleep(2500.millis) *> ZIO.succeed(s"interrupting task $id").debugThread *> fib.interrupt
      result <- fib.join
    } yield result

  def run = ???
}