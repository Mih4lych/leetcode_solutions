package learning.cats.rvjexercises

import cats.Semigroup
import cats.data.Validated
import learning.fppractice.NumberProblems._

object CatsExValidated extends App {
  // use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val listIsPrime = if (isPrime(n)) List.empty[String] else List("must be prime")
    val listNonNegative = if (n >= 0) listIsPrime else "must be non-negative" :: listIsPrime
    val listLessHundred = if (n <= 0) listNonNegative else "must be less or equal 100" :: listNonNegative
    val listEven = if (n % 2 == 0) listLessHundred else "must be even" :: listLessHundred

    listEven match {
      case Nil => Right(n)
      case _ => Left(listEven)
    }
  }

  //form validation
  object FormValidation {
    type FormValidation[A] = Validated[List[String], A]

    def validateField(form: Map[String, String], fieldName: String): FormValidation[String] = {
      Validated.fromOption(form.get(fieldName), List(s"$fieldName not specified"))
    }

    def validateEmptiness(value: String, fieldName: String): FormValidation[String] = {
      Validated.cond(value.nonEmpty, value, List(s"$fieldName is empty"))
    }

    def validateEmail(value: String): FormValidation[String] = {
      Validated.cond(value.contains('@'), value, List("email doesn't have @ symbol"))
    }

    def validatePassword(value: String): FormValidation[String] = {
      Validated.cond(value.length >= 10, value, List("password is too small"))
    }

    /*
          fields are
          - name
          - email
          - password
          rules are
          - name, email and password MUST be specified
          - name must not be blank
          - email must have "@"
          - password must have >= 10 characters
         */
    def validateFrom(form: Map[String, String]): FormValidation[String] = {
      validateField(form, "name").andThen(validateEmptiness(_, "name"))
        .combine(validateField(form, "email").andThen(validateEmptiness(_, "email")).andThen(validateEmail))
        .combine(validateField(form, "password").andThen(validateEmptiness(_, "password")).andThen(validatePassword))
        .map(_ => "Success")
    }
  }
  val testForm = Map(
    "name" -> "asd",
    "email" -> "asdasd@asd.pp",
    "password" -> "asdasdasdasdasdasd"
  )

  println(FormValidation.validateFrom(testForm))

}
