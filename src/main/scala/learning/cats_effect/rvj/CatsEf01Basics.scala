package learning.cats_effect.rvj

import scala.io.StdIn

object CatsEf01Basics extends App {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  def currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    for {
      start  <- currentTime
      _      <- computation
      finish <- currentTime
    } yield finish - start

  def print(line: String): MyIO[Unit] = MyIO(() => println(line))
  def readLine: MyIO[String] = MyIO(() => StdIn.readLine())
}
