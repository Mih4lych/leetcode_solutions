package learning.ziorvj

import zio._

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object ZIOEx9 extends ZIOAppDefault {

  /**
   * Exercises
   */
  // 1 - lift a computation running on some (external) thread to a ZIO
  // hint: invoke the cb when the computation is complete
  // hint 2: don't wrap the computation into a ZIO

  def external2ZIO[A](computation: () => A)(executor: ExecutorService): Task[A] = {
    ZIO.async[Any, Throwable, A] { cb =>
      executor.execute { () =>
        try {
          val res = computation()

          cb(ZIO.succeed(res))
        }
        catch {
          case e => cb(ZIO.fail(e))
        }
      }
    }
  }

  val executor = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(executor)

  // 2 - lift a Future to a ZIO
  // hint: invoke cb when the Future completes
  def future2ZIO[A](future: => Future[A])(implicit ec: ExecutionContext): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      future.onComplete {
        case Success(value) => cb(ZIO.succeed(value))
        case Failure(ex) => cb(ZIO.fail(ex))
      }
    }

  // 3 - implement a never-ending ZIO
  def neverEndingZIO[A]: UIO[A] =
    ZIO.async(_ => ())

  val neverEnd = ZIO.never
  def run = neverEnd


}
