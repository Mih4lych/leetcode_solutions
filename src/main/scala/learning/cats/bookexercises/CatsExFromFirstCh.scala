package learning.cats.bookexercises

import cats.{Eq, Show}
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._

object CatsExFromFirstCh {
  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit object PrinterForInt extends Printable[Int] {
      override def format(value: Int): String = s"$value is int"
    }

    implicit object PrinterForString extends Printable[String] {
      override def format(value: String): String = s"$value is string"
    }

    implicit object PrinterForCat extends Printable[Cat] {
      override def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat."
    }

    implicit val showForCat: Show[Cat] = Show.show[Cat] { cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat." }
    implicit val eqForCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) => cat1.age === cat2.age && cat1.name === cat2.name && cat1.color === cat2.color }
  }

  object Printable {
    def format[A](value: A)(implicit printer: Printable[A]): String = printer.format(value)
    def print[A : Printable](value: A): Unit = println(format(value))
  }


  object PrintableSyntax {
    implicit class PrintableOps[A](value: A)(implicit printer: Printable[A]) {
      def customFormat: String = printer.format(value)
      def print(): Unit = println(printer.format(value))
    }
  }

  final case class Cat(name: String, age: Int, color: String)

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    import PrintableSyntax._

    Printable.print(10)
    Printable.print("asd")

    println(10.customFormat)
    println("asdasd".customFormat)

    val cat = Cat(name = "Sam", age = 5, color = "black")

    cat.print()

    println(cat.show)

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(cat1 === cat2)
    println(optionCat1 === optionCat2)
  }
}
