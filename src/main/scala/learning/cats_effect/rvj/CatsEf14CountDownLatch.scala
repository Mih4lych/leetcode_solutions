package learning.cats_effect.rvj

import cats.effect.{Deferred, IO, IOApp}
import cats.effect.kernel.{Ref, Resource}
import cats.effect.std.CountDownLatch
import cats.syntax.parallel._
import cats.syntax.traverse._

import java.io.{File, FileWriter}
import scala.collection.immutable.Queue
import scala.io.Source

object CatsEf14CountDownLatch extends IOApp.Simple {
  object FileServer {
    val fileChunksList: Array[String] = Array(
      "I love Scala.",
      "Cats Effect seems quite fun.",
      "Never would I have thought I would do low-level concurrency WITH pure FP."
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }
  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) => IO(reader.getLines().foreach(writer.write)) >> IO(writer.write("\n"))
    }
  }

  /*
    - call file server API and get the number of chunks (n)
    - start a CDLatch
    - start n fibers which download a chunk of the file (use the file server's download chunk API)
    - block on the latch until each task has finished
    - after all chunks are done, stitch the files together under the same file on disk
   */
  def downloadFile(filename: String, destFolder: String): IO[Unit] = for {
    n <- FileServer.getNumChunks
    cdLatch <- CDLatch(n)
    _ <- (0 until n).toList.parTraverse(id => createFile(id, s"$destFolder/$filename", cdLatch))
    _ <- cdLatch.await
    _ <- (0 until n).toList.traverse(id => appendFileContents(s"$destFolder/$filename.part$id", s"$destFolder/$filename"))
  } yield ()

  def createFile(id: Int, path: String, cdLatch: CDLatch): IO[Unit] = for {
    chunk <- FileServer.getFileChunk(id)
    _ <- writeToFile(s"$path.part$id", chunk)
    _ <- cdLatch.release
  } yield ()

  override def run: IO[Unit] = downloadFile("myScalaTest.txt", "src/main/resources")

  /**
   * Exercise: implement your own CDLatch with Ref and Deferred.
   */

  abstract class CDLatch {
    def await: IO[Unit]
    def release: IO[Unit]
  }

  object CDLatch {
    def apply(n: Int): IO[CDLatch] = {
      for {
        signal <- Deferred[IO, Unit]
        ref <- Ref[IO].of(n)
      } yield new CDLatch {

        override def await: IO[Unit] = for {
          n <- ref.get
          _ <- if (n == 0) IO.unit else signal.get
        } yield ()

        override def release: IO[Unit] = IO.uncancelable(_ =>
          for {
            n <- ref.get
            _ <- if (n == 0) signal.complete(()) else ref.update(_ - 1)
          } yield ())
      }
    }
  }
}
