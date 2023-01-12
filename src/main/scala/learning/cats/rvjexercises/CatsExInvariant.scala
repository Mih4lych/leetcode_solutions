package learning.cats.rvjexercises

import cats.Monoid

object CatsExInvariant extends App {
  trait Crypto[A] {
    self =>
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B) = self.encrypt(back(value))

      override def decrypt(encrypted: String) = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String) = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String) = encrypted.map(c => (c - 2).toChar)
  }

  /*
    How can we support ints, doubles, Option[String]?
   */
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  //Option[String]

  implicit val optionCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  implicit def cryptoWrapper[T](value: T)(implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))
}
