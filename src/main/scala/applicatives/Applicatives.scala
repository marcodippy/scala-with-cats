package applicatives

import cats._
import cats.data._
import cats.implicits._

object SemigroupalsAndApplicatives extends App {

  case class Cat(name: String, born: Int, color: String)

  (Option("Garfield"), Option(1978)).tupled
  (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)

  //implement product in terms of flatMap
  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  type AllErrorsOr[A] = Validated[List[String], A]
  Semigroupal[AllErrorsOr].product(Validated.invalid(List("Error 1")), Validated.invalid(List("Error 2")))
}

object Test extends App {

  case class User(name: String, age: Int)

  type ErrorsOr[A] = Validated[List[String], A]

  def getValue(m: Map[String, String])(k: String): Either[List[String], String] =
    m.get(k).toRight(List(s"$k is mandatory"))

  def parseInt(s: String): Either[List[String], Int] =
    Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => List(s"$s is not an integer"))

  def nonBlank(s: String): Either[List[String], String] =
    s.asRight[List[String]].ensure(List("must be non blank"))(_.nonEmpty)

  def nonNegative(i: Int): Either[List[String], Int] =
    i.asRight[List[String]].ensure(List("must be non negative"))(_ >= 0)

  def readName(input: Map[String, String]): Either[List[String], String] =
    for {
      name <- getValue(input)("name")
      validName <- nonBlank(name)
    } yield validName

  def readAge(input: Map[String, String]): Either[List[String], Int] =
    for {
      ageStr <- getValue(input)("age")
      ageInt <- parseInt(ageStr)
      validAge <- nonNegative(ageInt)
    } yield validAge


  def readUser(input: Map[String, String]): ErrorsOr[User] =
    (readName(input).toValidated, readAge(input).toValidated).mapN((name, age) => User(name, age))

  println(readUser(Map("name" -> "Dave", "age" -> "37")))
  println(readUser(Map("age" -> "-1")))

}