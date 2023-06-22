package learning.cats_effect

import cats.data.OptionT
import cats.syntax.apply._
import cats.syntax.option._
import cats.effect._
import cats.syntax.applicative._
import cats.syntax.parallel._
import learning.cats_effect.utils.general.DebugWrapper

import java.io.{File, FileReader, FileWriter}
import java.util.Scanner
import scala.io.StdIn

object Playground extends IOApp.Simple {
  val testOnError = IO
    .raiseError(new RuntimeException("test"))
    .onError(_ => IO.println("test onError"))
    .void

  val test1 = OptionT(IO(println("asd")) *> IO(none[Int]))
  val test2 = OptionT.liftF(IO(println("asd"))) *> OptionT(IO(none[Int]))

  val testWithRef1 = Ref[IO].of(1).flatMap { ref =>
    (for {
      _ <- OptionT.liftF(ref.update(_ + 1) *> IO(none[Int]))
    } yield ref).value
  }

  val testWithRef2 = Ref[IO].of(1).flatMap { ref =>
    (for {
      _ <- OptionT.liftF(ref.update(_ + 1)) *> OptionT(IO(none[Int]))
    } yield ref).value
  }


  val testRef = Ref[IO].of(1).flatMap (_.modify(i => i + 1 -> i).replicateA(3)).debug

  override def run: IO[Unit] = testRef.void

  StdIn.readLine().split(" ").map(_.toInt).sum

  val writer = new FileWriter(new File("input.txt"))
  val reader = new Scanner(new FileReader(new File("input.txt")))

  writer.write(reader.nextLine().split(" ").map(_.toInt).sum)
  writer.close()
  reader.close()
}
