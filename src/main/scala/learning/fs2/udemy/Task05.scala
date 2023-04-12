package learning.fs2.udemy

import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Ref}
import fs2._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.reflect.ClassTag
import scala.util.Random

object Task05 extends IOApp.Simple {
  def compact[A : ClassTag](c: Chunk[A]): Chunk[A] = {
    val array = new Array[A](c.size)

    c.copyToArray(array)
    Chunk.array(array)
  }

  def skipLimit[A](skip: Int, limit: Int)(stream: Stream[IO, A]): Stream[IO, A] = {
    val p =
      for {
        sOpt <- stream.pull.drop(skip)
        _ <- sOpt match {
          case Some(s) => s.pull.take(limit)
          case None => Pull.done
        }
      } yield ()
    p.stream
  }

  def filter[A](p: A => Boolean): Pipe[Pure, A, A] = s => {
    def go(s: Stream[Pure, A]): Pull[Pure, A, Unit] = {
      s.pull.uncons1.flatMap {
        case Some((value, remStream)) =>
          if (p(value)) Pull.output1(value) >> go(remStream)
          else go(remStream)
        case None => Pull.done
      }
    }

    go(s).stream
  }

  def runningMax: Pipe[Pure, Int, Int] = s => {
    s.scanChunksOpt(Int.MinValue) { curMax =>
      Some { chunk =>
        val newMax = curMax.max(chunk.foldLeft(Int.MinValue)(_.max(_)))

        (newMax, Chunk.singleton(newMax))
      }
    }
  }

  def fetchRandomQuoteFromSource1: IO[String] = IO(Random.nextString(5))

  def fetchRandomQuoteFromSource2: IO[String] = IO(Random.nextString(25))

  //ex1
  val test =
    Stream.repeatEval(fetchRandomQuoteFromSource1).take(100)
      .merge(Stream.repeatEval(fetchRandomQuoteFromSource2).take(150))
      .interruptAfter(5.second)
      .compile
      .toList
      .flatMap(IO.println)

  //ex2
  def producer(id: Int, queue: Queue[IO, Int]): Stream[IO, Nothing] = {
    Stream.repeatEval(queue.offer(id)).drain
  }

  def consumer(id: Int, queue: Queue[IO, Int]): Stream[IO, Nothing] = {
    Stream.repeatEval(queue.take).map(i => s"Consumer $id consumes $i").printlns
  }

  val ex2 =
    Stream.eval(Queue.unbounded[IO, Int]).flatMap { queue =>
      val producers =
        Stream
          .range(0, 5)
          .map(id => producer(id, queue))

      val consumers =
        Stream
          .range(0, 10)
          .map(id => consumer(id, queue))

      (producers ++ consumers).parJoinUnbounded
    }.interruptAfter(5.seconds).compile.drain

  //ex4
  val numItems = 30

  def processor(processedItem: Ref[IO, Int]): Stream[IO, Nothing] = {
    Stream
      .repeatEval(processedItem.update(_ + 1))
      .take(numItems)
      .metered(100.millis)
      .drain
  }

  def progressTracker(processedItem: Ref[IO, Int]): Stream[IO, Nothing] = {
    Stream
      .repeatEval(processedItem.get.flatMap(item => IO.println(s"Progress: ${item * 100 / numItems}")))
      .metered(100.millis)
      .drain
  }

  val testConcurrent: IO[Unit] =
    Stream
      .eval(Ref.of[IO, Int](0))
      .flatMap(ref => processor(ref).concurrently(progressTracker(ref)))
      .compile
      .drain

  //ex5
  case class Event(jobId: Long, seqNo: Long)

  sealed trait JobState
  case object Created extends JobState
  case object Processed extends JobState

  case class Job(id: Long, state: JobState)

  def processJob(job: Job): IO[Job] = {
    IO.println(s"start processing ${job.id}") *>
      IO.sleep(1.second) *>
      IO.pure(job.copy(state = Processed))
  }


  def processJobS(job: Job): IO[List[Event]] = {
    IO.println(s"start processing ${job.id}") *>
      IO.sleep(1.second) *>
      IO.pure(List.range(1, 10).map(seqNo => Event(jobId = job.id, seqNo = seqNo)))
  }

  implicit class RichStream[A](s: Stream[IO, A]) {
    def parEvalMapSeq[B](maxConcurrent: Int)(f: A => IO[List[B]]): Stream[IO, B] =
      s
        .parEvalMap(maxConcurrent)(f)
        .flatMap(Stream.emits)
    def parEvalMapSeqUnbounded[B](f: A => IO[List[B]]): Stream[IO, B] =
      s
        .parEvalMapUnbounded(f)
        .flatMap(Stream.emits)
  }

  //ex6
  def metered[A](s: Stream[IO, A], d: FiniteDuration): Stream[IO, A] = {
    Stream.fixedRate[IO](d).zipRight(s)
  }

  //ex7
  def spaced[A](s: Stream[IO, A], d: FiniteDuration): Stream[IO, A] = {
    Stream.fixedDelay[IO](d).zipRight(s)
  }

  override def run: IO[Unit] = {
    IO.println(compact(Chunk.singleton(10)))

    (Stream(1, 2) ++ Stream(3) ++ Stream(4, 5))
      .through(filter(_ % 2 == 0))
      .through(runningMax)
      .evalMap(IO.println)
      .compile
      .drain

    testConcurrent
  }
}
