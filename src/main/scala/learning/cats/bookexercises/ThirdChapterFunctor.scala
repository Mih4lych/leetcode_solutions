package learning.cats.bookexercises

object ThirdChapterFunctor extends App {
  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = { value => self.format(func(value)) }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  case class Box[A](value: A)

  implicit val booleanPrintable: Printable[Boolean] = { value => if (value) "Yes" else "No" }
  implicit val intPrintable: Printable[Int] = _.toString
  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] = printable.contramap(_.value)

  println(format(Box(123)))
  println(format(Box(false)))
  //println(format(Box("false")))

  trait Codec[A] { self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] =
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))
        override def decode(value: String): B = dec(self.decode(value))
      }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val doubleCodec: Codec[Double] =
    new Codec[Double] {
      override def encode(value: Double): String = value.toString

      override def decode(value: String): Double = value.toDouble
    }

  println(encode(10.4))
  println(decode[Double]("10.4"))

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] = {
    codec.imap(Box(_), _.value)
  }

  println(encode(Box(11.1)))
  println(decode[Box[Double]]("11.1"))
}
