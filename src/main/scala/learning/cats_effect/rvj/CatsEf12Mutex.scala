package learning.cats_effect.rvj

import cats.effect.{Deferred, IO, Ref}

import scala.collection.immutable.Queue

object CatsEf12Mutex {
  abstract class Mutex {
    def acquire: IO[Unit]

    def release: IO[Unit]
  }

  object Mutex {
    type Signal = Deferred[IO, Unit]

    case class State(locked: Boolean, waiting: Queue[Signal])

    val unlocked = State(locked = false, Queue())

    def createSignal(): IO[Signal] = Deferred[IO, Unit]

    def create: IO[Mutex] = Ref[IO].of(unlocked).map(createMutexWithCancellation)

    def createMutexWithCancellation(state: Ref[IO, State]): Mutex =
      new Mutex {
        override def acquire: IO[Unit] = ???

        override def release: IO[Unit] = ???
      }
    def createSimpleMutex(state: Ref[IO, State]): Mutex = new Mutex {
      /*
        Change the state of the Ref:
        - if the mutex is currently unlocked, state becomes (true, [])
        - if the mutex is locked, state becomes (true, queue + new signal) AND WAIT ON THAT SIGNAL.
       */
      override def acquire = IO.uncancelable { poll =>
        createSignal().flatMap { signal =>
          val cleanup = state.modify {
            case State(locked, queue) =>
              val newQueue = queue.filterNot(_ eq signal)

              State(locked = locked, waiting = newQueue) -> release
          }.flatten

          state.modify {
            case State(false, _) => State(locked = true, Queue()) -> IO.unit
            case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
          }.flatten // modify returns IO[B], our B is IO[Unit], so modify returns IO[IO[Unit]], we need to flatten
        }
      }

      /*
        Change the state of the Ref:
        - if the mutex is unlocked, leave the state unchanged
        - if the mutex is locked,
          - if the queue is empty, unlock the mutex, i.e. state becomes (false, [])
          - if the queue is not empty, take a signal out of the queue and complete it (thereby unblocking a fiber waiting on it)
       */
      override def release = state.modify {
        case State(false, _) => unlocked -> IO.unit
        case State(true, queue) =>
          if (queue.isEmpty) unlocked -> IO.unit
          else {
            val (signal, rest) = queue.dequeue
            State(locked = true, rest) -> signal.complete(()).void
          }
      }.flatten
    }
  }
}
