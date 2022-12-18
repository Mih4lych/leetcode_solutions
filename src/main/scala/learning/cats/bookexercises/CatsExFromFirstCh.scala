package learning.cats.bookexercises

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
  }
}
