import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success}

object Tests extends App {
  def compose[A, B, C]
  (g: B => C, f: A => B): A => C = a => g(f(a))

  def fuse[A, B]
  (a: Option[A], b: Option[B]): Option[(A, B)] = {
    (a, b) match {
      case (Some(first), Some(second)) => Some(first, second)
      case _ => None
    }
  }

  @tailrec
  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = {
    xs match {
      case Nil => true
      case h :: t => if (pred(h)) check(t)(pred) else false
    }
  }

  def permutations(x: String): Seq[String] = {
    x.toSeq.permutations.map(_.unwrap).toSeq
  }

  def permutationsByHands(x: String): Seq[String] = {
    def loop(xs: Seq[Char] = x.toSeq): Seq[String] = {
      xs match {
        case Nil => Nil
        case Seq(head) => Seq(head.toString)
        case _ =>
          for {
            s <- xs
            difStr <- loop(xs.diff(Seq(s)))
          } yield s +: difStr
      }
    }

    loop()
  }

  val testThread = new Thread { () =>
    println("hello")
  }

  println(0 % 1)

  val cardOfBoard = "5c6dAcAsQs".grouped(2).toList

  val boardsCombinations = (for {
    h1 :: t1 <- cardOfBoard.tails
    h2 :: t2 <- t1.tails
    h3 <- t2
  } yield s"$h1$h2$h3").toList

  println(boardsCombinations.mkString(" "))

  import scala.util.chaining._

  def multi3(list: List[Int]): List[Int] = list.map(_ * 3)

  @tailrec
  def max(list: List[Int]): Int = max(list)

  //List(1, 2, 3).pipe(multi3).pipe(max).pipe(_ * 3)

  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val source = Source.fromURL("https://google.com")
  try {
    val regex = """(http|ftp|https):\/\/([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:\/~+#-]*[\w@?^=%&\/~+#-])""".r

    println(regex.findAllIn(source.mkString).toList)
  } finally {
    source.close()
  }

  println((Map('a' -> 1, 'b' -> 2, 'c' -> 1).toSet diff Map('a' -> 2, 'b' -> 2, 'd' -> 1).toSet).size)
}
