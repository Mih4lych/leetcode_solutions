object Tests extends App{
  def compose[A, B, C]
  (g: B => C, f: A => B): A => C = a => g(f(a))

  def fuse[A, B]
  (a: Option[A], b: Option[B]): Option[(A, B)] = {
    (a, b) match {
      case (Some(first), Some(second)) => Some(first, second)
      case _ => None
    }
  }

  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = {
    xs match {
      case Nil => true
      case h::t => if (pred(h)) check(t)(pred) else false
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

  val testThread = new Thread {() => println("hello")}

  println(0 % 1)
}
