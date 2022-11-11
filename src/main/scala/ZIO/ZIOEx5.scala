package ZIO

import zio._

import java.io.{File, FileReader, FileWriter}

object ZIOEx5 extends ZIOAppDefault {
  /**
   * Exercises
   */
  // 1 - zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both
  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E,(A, B)]] = {
    (for {
      result1 <- fiber1.join
      result2 <- fiber2.join
    } yield (result1, result2)).foldZIO(
     fl => ZIO.fail(fl).fork,
     fr => ZIO.succeed(fr).fork
   )
  }

  def betterZipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E,(A, B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  val zippedFibers_v2 = for {
    fib1 <- ZIO.succeed("Result from fiber 1").fork
    fib2 <- ZIO.succeed("Result from fiber 2").fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  // 2 - same thing with orElse

  def chainFibers[E,A](fiber1: Fiber[E,A], fiber2: Fiber[E,A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    fiber1.join.orElse(fiber2.join).fork
  }

  // 3 - distributing a task in between many fibers
  // spawn n fibers, count the n of words in each file,
  // then aggregate all the results together in one big number

  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 random words

    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  def counter(path: String): UIO[Int] = {
    ZIO.succeed{
      val fileReader = scala.io.Source.fromFile(path)
      val wordsCount = fileReader.getLines().mkString(" ").split(" ").count(_.nonEmpty)

      fileReader.close()
      wordsCount
    }
  }

  val res = {
    (1 to 10)
      .map(i => s"src/main/resources/testfile_$i.txt")
      .map(counter)
      .map(_.fork)
      .map(_.flatMap(_.join))
      .reduce {(zioA, zioB) =>
      for {
        resA <- zioA
        resB <- zioB
      } yield resA + resB
    }
  }

  def run = res.tap(nums => ZIO.succeed(println(nums)))
}
