package learning.akka.rvj

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, ActorSystem}

object Ex04AnotherChild extends App {

  trait MasterProtocol
  trait WorkerProtocol
  trait UserProtocol

  case class Initialize(nChild: Int) extends MasterProtocol
  case class WordCountTask(text: String, replyTo: ActorRef[UserProtocol]) extends MasterProtocol
  case class WordCountReply(id: Int, count: Int) extends MasterProtocol

  case class WorkerTask(id: Int, text: String) extends WorkerProtocol

  case class Reply(count: Int) extends UserProtocol
  object WordCounterMaster {
    def apply(): Behavior[MasterProtocol] = Behaviors.receive { (context, msg) =>
      msg match {
        case Initialize(nChild) =>
          context.log.info(s"Initialize $nChild workers")
          active(nChild
            , (0 until nChild)
              .foldRight(Seq.empty[ActorRef[WorkerProtocol]])
              ((id, acc) => context.spawn(WordCounterWorker(context.self), s"worker$id") +: acc)
          )
      }
    }

    def active(workerCount: Int
               , workers: Seq[ActorRef[WorkerProtocol]]
               , nextWorkerId: Int = 0
               , nextUserRefId: Int = 0
               , replies: Map[Int, ActorRef[UserProtocol]] = Map.empty
               , total: Int = 0): Behavior[MasterProtocol] = {
      Behaviors.receive { (context, msg) =>
        msg match {
          case Initialize(_) =>
            context.log.error("Workers already initialized")
            active(workerCount, workers, nextWorkerId, nextUserRefId, replies, total)
          case WordCountReply(id, count) =>
            replies.get(id) match {
              case Some(ref) =>
                context.log.info("Form the response")
                ref ! Reply(total + count)
                active(workerCount, workers, nextWorkerId, nextUserRefId, replies.removed(id), total + count)
              case None =>
                context.log.error("Response is missing")
                active(workerCount, workers, nextWorkerId, nextUserRefId, replies, total)
            }
          case WordCountTask(text, replyTo) =>
            val newReplies = replies.updated(nextUserRefId, replyTo)

            context.log.info(s"Start work on $nextWorkerId")
            workers(nextWorkerId) ! WorkerTask(nextUserRefId, text)

            active(workerCount, workers, (nextWorkerId + 1) % workerCount, nextUserRefId + 1, newReplies, total)
          case _ =>
            context.log.error("Response is missing")
            active(workerCount, workers, nextWorkerId, nextUserRefId, replies, total)
        }
      }
    }
  }

  object WordCounterWorker {
    def apply(masterRef: ActorRef[MasterProtocol]): Behavior[WorkerProtocol] = Behaviors.receive { (context, msg) =>
      msg match {
        case WorkerTask(id, text) =>
          context.log.info("Start counting")
          val count = text.split(" ").length

          masterRef ! WordCountReply(id, count)
          Behaviors.same
        case _ =>
          context.log.error("Unsupported operation")
          Behaviors.same
      }
    }
  }

  object Aggregator {
    def apply(): Behavior[UserProtocol] = active()

    def active(totalWord: Int = 0): Behavior[UserProtocol] = Behaviors.receive { (context, msg) =>
      msg match {
        case Reply(count) =>
          context.log.info(s"count $count")
          active(totalWord + count)
      }
    }
  }

  def test(): Unit = {
    val userGuardian: Behavior[Unit] = Behaviors.setup { context =>
      val agg = context.spawn(Aggregator(), "agg")
      val wcm = context.spawn(WordCounterMaster(), "master")

      wcm ! Initialize(3)
      wcm ! WordCountTask("asd asd asd", agg)
      wcm ! WordCountTask("asd asd asd", agg)
      wcm ! WordCountTask("asd asd asd", agg)

      Behaviors.empty
    }

    val system = ActorSystem(userGuardian, "WordCounting")
    Thread.sleep(1000)
    system.terminate()
  }

  test()
}
