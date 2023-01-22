package learning.zio.ziorvj

import sun.misc.Signal
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

  val beginState = State()

  def make: UIO[Mutex] = Ref.make(beginState).map { state =>
    new Mutex {

      /*
        Change the state of the Ref
        - if the mutex is unlocked, lock it
        - if the mutex is locked, state becomes (true, queue + new signal) and WAIT on that signal
       */
      override def acquire: UIO[Unit] = {
        Promise.make[Nothing, Unit].flatMap { signal =>
          state.modify {
            case State(false, _) => ZIO.unit -> State(true, Queue())
            case State(true, waiting) => signal.await -> State(true, waiting.enqueue(signal))
          }.flatten
        }
      }

      /*
              Change the state of the Ref
              - if the mutex is unlocked, leave the state unchanged
              - if the mutex is locked
                - if the queue is empty, unlock the mutex
                - if the queue is non-empty, take a signal out of the queue and complete it
             */
      override def release: UIO[Unit] = {
        state.modify {
          case State(false, _) => ZIO.unit -> beginState
          case State(true, waiting) =>
            if (waiting.isEmpty)
              ZIO.unit -> beginState
            else {
              val (prom, restQueue) = waiting.dequeue
              prom.succeed(()).unit -> State(true, restQueue)
            }
        }.flatten
      }
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
    for {
      _ <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
      _ <- mutex.acquire
      // critical region start
      _ <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion()
      _ <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
      // critical region end
      _ <- mutex.release
    } yield result
  }

  def demoLockingTasks() = for {
    mutex <- Mutex.make
    _ <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def run = demoLockingTasks()
}