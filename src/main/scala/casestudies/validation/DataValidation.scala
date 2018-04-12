package casestudies.validation

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._


sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(cf) => cf(a)
      case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
      case Or(l, r) => l(a) match {
        case Valid(vla) => Valid(vla)
        case Invalid(le) => r(a) match {
          case Valid(vra) => Valid(vra)
          case Invalid(re) => Invalid(le |+| re)
        }
      }
    }

  def check: Check[E, A, A] = Check(this)

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    a => this (a).toEither
}

object Predicate {
  def pure[E, A](cf: A => Validated[E, A]): Predicate[E, A] = Pure(cf)

  case class And[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  case class Or[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  case class Pure[E, A](cf: A => Validated[E, A]) extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
}


sealed trait Check[E, A, B] {

  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, that)
}


object Check {

  case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(f)
  }

  case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  case class AndThen[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      c1(a).withEither(_.flatMap(b => c2(b).toEither))
  }

  case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = func(a)
  }

  case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)
}

object Validation {
  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.count(c => c == char) == 1)
}

object DataValidation extends App {

  import Validation._

  def checkUsername: Check[Errors, String, String] =
    Check(alphanumeric and longerThan(3))

  val checkEmail: Check[Errors, String, String] =
    Check(containsOnce('@'))
      .flatMap(email =>
        Check((_: String) => email.split('@') match {
          case Array(name, domain) =>
            (longerThan(0).check(name), (longerThan(3) and contains('.')).check(domain)).mapN((_, _) => email)
          case _ => "Invalid email".invalidNel[String]
        })
      )

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  println(createUser("", "dave@underscore@io"))
}

object ValidationWithKleisli extends App {

  import Validation._

  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)


  def checkUsername: Check[String, String] =
    checkPred(alphanumeric and longerThan(3))

  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) =>
        Right((name, domain))
      case _ =>
        Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] = checkPred(longerThan(0))

  val checkRight: Check[String, String] = checkPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] =
    check { case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _) }

  val checkEmail: Check[String, String] = splitEmail andThen joinEmail
}