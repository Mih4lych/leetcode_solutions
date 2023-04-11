package learning.fs2.udemy

import cats.effect.{IO, IOApp}
import fs2._

import scala.reflect.ClassTag

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

  def runnigMax: Pipe[Pure, Int, Int] = s => {
    s.scanChunksOpt(Int.MinValue) { curMax =>
      Some { chunk =>
        val newMax = curMax.max(chunk.foldLeft(Int.MinValue)(_.max(_)))

        (newMax, Chunk.singleton(newMax))
      }
    }
  }

  override def run: IO[Unit] = {
    IO.println(compact(Chunk.singleton(10)))

    (Stream(1, 2) ++ Stream(3) ++ Stream(4, 5))
      .through(filter(_ % 2 == 0))
      .through(runnigMax)
      .evalMap(IO.println)
      .compile
      .drain
  }
}
