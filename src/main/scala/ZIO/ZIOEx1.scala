package ZIO

import scala.io.StdIn

object ZIOEx1 {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  /**
   * Exercises - create some IO which
   *  1. measure the current time of the system
   *     2. measure the duration of a computation
   *    - use exercise 1
   *    - use map/flatMap combinations of MyIO
   *      3. read something from the console
   *      4. print something to the console (e.g. "what's your name"), then read, then print a welcome message
   */
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = for {
    startTime <- currentTime
    result <- computation
    endTime <- currentTime
  } yield (endTime - startTime, result)

  def read: MyIO[String] = {
    MyIO(() => StdIn.readLine())
  }

  def print(name: String): MyIO[Unit] = MyIO(() => println(s"My name is $name"))

  def readAndPrint: MyIO[Unit] = for {
    line <- read
    _ <- print(line)
  } yield ()

  println("Test")
}
