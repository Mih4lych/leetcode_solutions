package learning.ziorvj

import zio.{Cause, _}

object ZIOEx6 extends ZIOAppDefault {
  /**
   * Exercise
   */
  /*
    1 - implement a timeout function
      - if zio is successful before timeout => a successful effect
      - if zio fails before timeout => a failed effect
      - if zio takes longer than timeout => interrupt the effect
   */

  def timeout[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,A] = {
    for {
      fiber <- zio.fork
      _ <- (ZIO.sleep(time) *> fiber.interrupt).fork
      result <- fiber.join
    } yield result
  }

  /*
      2 - timeout v2
        - if zio is successful before timeout => a successful effect with Some(a)
        - if zio fails before timeout => a failed effect
        - if zio takes longer than timeout => interrupt the effect, return a successful effect with None
        // hint: foldCauseZIO
     */
  def timeout_v2[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,Option[A]] = {
    for {
      fiber <- zio.fork
      _ <- (ZIO.sleep(time) *> fiber.interrupt).fork
      result <- fiber.join.foldCauseZIO(
        cause => if (cause.isInterrupted) ZIO.succeed(None) else ZIO.failCause(cause)
        ,success => ZIO.succeed(Some(success)))
    } yield result
  }

  def run = timeout_v2(ZIO.succeed(println("Start"))
    *> ZIO.sleep(3.seconds)
    *> ZIO.succeed("Finish"), 3.seconds).map(println)
}
