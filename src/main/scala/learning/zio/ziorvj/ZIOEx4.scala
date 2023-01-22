package learning.zio.ziorvj

import java.io.IOException
import zio._

object ZIOEx4 {
  // 1 - make this effect fail with a TYPED error
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("this is bad!"))
  val recover = aBadFailure.sandbox.mapError(_ => new RuntimeException)

  val aBetterFailure = aBadFailure.sandbox // exposes the defect in the Cause
  val aBetterFailure_v2 = aBadFailure.unrefine { // surfaces out the exception in the error channel
    case e => e
  }

  // 2 - transform a zio into another zio with a narrower exception type
  def ioException[R,A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie[IOException] {
      case e: IOException => e
    }

  def left[R, E, A, B](zio: ZIO[R,E,Either[A,B]]): ZIO[R, Either[E,A], B] = {
    zio.foldZIO(fa => ZIO.fail(Left(fa)), {
      case Left(e) => ZIO.fail(Right(e))
      case Right(value) => ZIO.succeed(value)
    })
  }

  // 4
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )
  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase())
      ZIO.fail(QueryError("user ID format is invalid"))
    else
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): IO[Either[QueryError, Option[Nothing]], UserProfile] = {
    if (userId.isEmpty)
      ZIO.fail(Left(QueryError("user ID is empty")))
    if (userId != userId.toLowerCase())
      ZIO.fail(Left(QueryError("user ID format is invalid")))
    else
      database.get(userId) match {
        case Some(value) => ZIO.succeed(UserProfile(userId, value))
        case _ => ZIO.fail(Right(None))
      }
  }
}
