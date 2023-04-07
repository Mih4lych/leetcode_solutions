package learning.fs2.udemy

import cats.effect.{IO, IOApp}
import fs2.Chunk

import scala.reflect.ClassTag

object Task05 extends IOApp.Simple {
  def compact[A : ClassTag](c: Chunk[A]): Chunk[A] = {
    val array = new Array[A](c.size)

    c.copyToArray(array)
    Chunk.array(array)
  }

  override def run: IO[Unit] = IO.println(compact(Chunk.singleton(10)))
}
