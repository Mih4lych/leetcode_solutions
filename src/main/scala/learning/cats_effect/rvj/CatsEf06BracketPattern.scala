package learning.cats_effect.rvj

import cats.effect.{IO, IOApp}
import learning.cats_effect.utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object CatsEf06BracketPattern extends IOApp.Simple {
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path)
      .bracket {scanner =>
        def readNextLine(): IO[Unit] = {
          if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millisecond) >> readNextLine()
          else IO.unit
        }

        readNextLine()
      } (scanner => IO(scanner.close()))
  }

  override def run: IO[Unit] =
    bracketReadFile("src/main/scala/learning/cats_effect/rvj/CatsEf05Fibers.scala")
}
