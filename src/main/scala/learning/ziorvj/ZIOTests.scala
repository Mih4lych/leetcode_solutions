package learning.ziorvj

import zio._
import utils._

object ZIOTests extends ZIOAppDefault {

  val zio1 = ZIO.sleep(1.seconds) *> ZIO.succeed("1").map(println)
  val zio2 = ZIO.sleep(1.seconds) *> ZIO.succeed("2").map(println)

  val checkUninterruption = (zio1 *> zio2).uninterruptible
  val checkInterruption = (zio1 *> zio2.interruptible).uninterruptible

  val testInterruption1 = for {
    fib <- checkUninterruption.fork
    _ <- ZIO.sleep(500.millis)
    _ <- fib.interrupt
    _ <- fib.join
  } yield ()

  val testInterruption2 = for {
    fib <- checkInterruption.fork
    _ <- ZIO.sleep(500.millis)
    _ <- fib.interrupt
    _ <- fib.join
  } yield ()

  val checkInterruptionByParrent = for {
    fib <- (ZIO.sleep(1.seconds) *> ZIO.succeed(println("I'm waiting another second")) *>
      ZIO.sleep(1.seconds) *>
      ZIO.succeed(println("I'm done"))).uninterruptible.fork
    _ <- fib.interrupt
    _ <- fib.join
  } yield ()

  //example of authorization system

  val inputPassword = (for {
    _ <- ZIO.succeed("Input password:").debugThread
    _ <- ZIO.succeed("(typing password)").debugThread
    _ <- ZIO.sleep(2.seconds)
    pass <- ZIO.succeed("RockTheJVM1!")
  } yield pass).interruptible

  def verifyPassword(pw: String) = for {
    _ <- ZIO.succeed("verifying...").debugThread
    _ <- ZIO.sleep(2.seconds)
    result <- ZIO.succeed(pw == "RockTheJVM1!")
  } yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    // EVERYTHING is uninterruptible...
    for {
      pw <- inputPassword /* <- ... except this thing */ .onInterrupt(ZIO.succeed("Authentication timed out. Try again later.").debugThread)
      // ^^ restores the interruptibility flag of this ZIO at the time of the call
      verification <- verifyPassword(pw)
      _ <- if (verification) ZIO.succeed("Authentication successful.").debugThread
      else ZIO.succeed("Authentication failed.").debugThread
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()

  val cancelBeforeMol = ZIO.interrupt *> ZIO.succeed(42).debugThread
  val uncancelBeforeMol = ZIO.uninterruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)
  def run = uncancelBeforeMol
}
