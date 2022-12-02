package learning.concurrency

import scala.collection.mutable

object ConcurrencyPractice extends App{
  def parallel[A, B](a: =>A, b: =>B): (A, B) = {
    var resA: Option[A] = None
    var resB: Option[B] = None
    val thread1 = new Thread(() => resA = Some(a))
    val thread2 = new Thread(() => resB = Some(b))

    thread1.start()
    thread2.start()

    thread1.join()
    thread2.join()

    (resA.get, resB.get)
  }

  def periodically(duration: Long)(b: =>Unit): Unit = {
    val thread = new Thread { () =>
      while (true) {
        b
        Thread.sleep(duration)
      }
    }

    thread.start()

    thread.join()
  }

  class SyncVar[T] {
    private var variant: Option[T] = None

    def get(): T = {
      synchronized {
        variant match {
          case Some(v) =>
            variant = None
            v
          case None => throw new NoSuchElementException()
        }
      }
    }

    def put(x: T): Unit = {
      synchronized {
        variant match {
          case Some(_) => throw new IllegalStateException()
          case None => variant = Some(x)
        }
      }
    }

    def getWait(): T = {
      synchronized {
        variant match {
          case Some(v) =>
            variant = None
            notify()
            v
          case None =>
            wait()
            getWait()
        }
      }
    }

    def putWait(x: T): Unit = {
      synchronized {
        variant match {
          case Some(_) =>
            wait()
            putWait(x)
          case None =>
            variant = Some(x)
            notify()
        }
      }
    }

    def isEmpty: Boolean = synchronized(variant.isEmpty)
    def nonEmpty: Boolean = synchronized(variant.nonEmpty)
  }

  class SyncQueue[A](n: Int) {
    private val queue = new mutable.Queue[A]()

    def isEmpty: Boolean = queue.isEmpty
    def isFull: Boolean = queue.size == n

    def putWait(n: A): Unit = queue.synchronized {
      while (isFull) wait()

      queue.enqueue(n)
      notify()
    }

    def getWait(): A = queue.synchronized {
      while (isEmpty) wait()

      val n = queue.dequeue()
      notify()
      n
    }
  }
}
