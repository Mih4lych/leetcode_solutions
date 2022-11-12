package ZIO

import zio._

object ZIOEx7 extends ZIOAppDefault{

  // try a zipPar combinator
  // hint: fork/join/await, interrupt
  def myZipPar[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, (A, B)] = {
    val exits = for {
      fiber1 <- zioa.fork
      fiber2 <- ziob.fork
      exit1 <- fiber1.await
      exit2 <- exit1 match {
        case Exit.Success(_) => fiber2.await
        case Exit.Failure(_) => fiber2.interrupt
      }
    } yield (exit1, exit2)

    exits.flatMap {
      case (Exit.Failure(e1), Exit.Failure(e2)) => ZIO.failCause(e1 && e2)
      case (Exit.Failure(e), _) => ZIO.failCause(e)
      case (_, Exit.Failure(e)) => ZIO.failCause(e)
      case (Exit.Success(v1), Exit.Success(v2)) => ZIO.succeed((v1, v2))
    }
  }

  /**
   * Exercise: do the word counting exercise, using a parallel combinator
   */
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }

  val seqZIO = (1 to 10)
    .map(i => s"src/main/resources/testfile_$i.txt")
    .map(countWords)

  val firsRes = ZIO.reduceAllPar(ZIO.succeed(0), seqZIO)(_ + _)
  val secondRes = ZIO.mergeAllPar(seqZIO)(0)(_ + _)
  val thirdRes = ZIO.collectAllPar(seqZIO).map(_.sum)

  def run = firsRes.map(println) *> secondRes.map(println) *> thirdRes.map(println)
}
