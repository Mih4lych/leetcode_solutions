package ZIO

import zio._

object ZIOEx2 {
  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,B] = {
    zioa.flatMap(_ => ziob)
  }

  // 2 - sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,A] = {
    zioa <* ziob
  }

  def sequenceTakeFirst_v2[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,A] = {
    ziob.flatMap(_ => zioa)
  }

  def sequenceTakeFirst_v3[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,A] = {
    for {
      res <- zioa
      _ <- ziob
    } yield res
  }

  // 3 - run a ZIO forever
  def runForever[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,A] = {
    zio *> runForever(zio)
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R,E,A,B](zio: ZIO[R,E,A], value: B): ZIO[R,E,B] = {
    zio.map(_ => value)
  }
  // 5 - discard the value of a ZIO to Unit
  def asUnit[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,Unit] = {
    zio.map(_ => ())
  }

  // 6 - recursion
  def sumZIO(n: Int): UIO[Int] = {
    if (n == 0) ZIO.succeed(0)
    else {
      for {
        cur <- ZIO.succeed(n)
        next <- sumZIO(n - 1)
      } yield cur + next
    }
  }

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty

    Unsafe.unsafe { implicit u =>
      println(runtime.unsafe.run(sumZIO(20000)))
    }
  }
}
