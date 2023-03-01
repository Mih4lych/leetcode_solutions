package learning.cats_effect.rvj

import cats.effect.{IO, IOApp, Resource}
import learning.cats_effect.utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object CatsEf07Resource extends IOApp.Simple {
  def openFileScanner(path: String): Resource[IO, Scanner] =
    Resource.make(IO(new Scanner(new FileReader(new File(path)))))(scanner => IO(scanner.close()))

  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path).use { scanner =>
      def readNextLine(): IO[Unit] = {
        if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millisecond) >> readNextLine()
        else IO.unit
      }

      readNextLine()
    }
  }

  override def run: IO[Unit] =
    bracketReadFile("src/main/scala/learning/cats_effect/rvj/CatsEf05Fibers.scala")
}
