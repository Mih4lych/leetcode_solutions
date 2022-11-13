package learning.ziorvj

import zio._

import java.io.File
import java.util.Scanner

object ZIOEx8 extends ZIOAppDefault {

  /**
   * Exercises
   * 1. Use the acquireRelease to open a file, print all lines, (one every 100 millis), then close the file
   */
  def openFileScanner(path: String): UIO[Scanner] = {
    ZIO.succeed(new Scanner(new File(path)))
  }

  def readLineByLine(scanner: Scanner): UIO[Unit] =
    if (scanner.hasNextLine) {
      ZIO.succeed(println(scanner.nextLine())) *> ZIO.sleep(100.millis) *> readLineByLine(scanner)
    }
    else {
      ZIO.unit
    }

  def acquireOpenFile(path: String): UIO[Unit] = {
    ZIO.acquireReleaseWith(
      openFileScanner(path)
    )(scanner =>
      ZIO.succeed(println("Closed")) *> ZIO.succeed(scanner.close())
    )(readLineByLine)
  }

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/ZIO/ZIOEx8.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  def run = testInterruptFileDisplay
}
