package monads

import cats._
import cats.data._
import cats.implicits._

object Exercise {

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

}

object Monads {
  val idMonad: Exercise.Monad[Id] = new Exercise.Monad[Id] {
    override def pure[A](a: A): Id[A] = a

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa): Id[B]
  }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, evalB) => evalB.map(b => fn(a, b)) }.value
}


object WriterTest extends App {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  // Await.result(Future.sequence(Vector(Future(factorial(3)), Future(factorial(3)))), 5.seconds)

  type FactorialResult[A] = Writer[Vector[String], A]

  def factorialW(n: Int): FactorialResult[Int] =
    for {
      a <- slowly[FactorialResult[Int]] {
        if (n == 0) 1.pure[FactorialResult] else factorialW(n - 1).map(_ * n)
      }
      _ <- Vector(s"fact $n $a").tell
    } yield a

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(Future(factorialW(3)), Future(factorialW(3)))), 5.seconds).foreach(result => result.written.foreach(println))
}


object ReaderTest extends App {

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader((db: Db) => db.passwords.get(username))
      .map(opPass => opPass.contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      userOp <- findUsername(userId)
      validPasswd <- userOp.map(user => checkPassword(user, password)).getOrElse(false.pure[DbReader])
    } yield validPasswd

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))

}

object StateTest extends App {
  type CalcState[A] = State[List[Int], A]

  def operand(o: Int): CalcState[Int] =
    State(stack => (o :: stack, o))

  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State {
      case a :: b :: tail => {
        val r = f(a, b)
        (r :: tail, r)
      }
      case _ => throw new IllegalArgumentException("boom")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "*" => operator(_ * _)
      case "+" => operator(_ + _)
      case "/" => operator(_ / _)
      case "-" => operator(_ - _)
      case _ => operand(sym.toInt)
    }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(program.runA(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((st, s) => st.flatMap(_ => evalOne(s)))

  println(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)

  val program2 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(program2.runA(Nil).value)

  def evalInput(expr: String): CalcState[Int] =
    evalAll(expr.split(' ').toList)

  println(evalInput("1 2 + 3 4 + *").runA(Nil).value)

}

object CustomMonads extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)


  val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      case Leaf(a) => f(a)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Branch(l, r) => branch(
        flatMap(l) {
          case Left(ll) => tailRecM(ll)(f)
          case Right(rr) => pure(rr)
        },
        flatMap(r) {
          case Left(ll) => tailRecM(ll)(f)
          case Right(rr) => pure(rr)
        }
      )
      case Leaf(Left(aa)) => tailRecM(aa)(f)
      case Leaf(Right(b)) => leaf(b)
    }
  }
}
