package functors

import cats._
import cats.implicits._

object Functors extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](a: A): Tree[A] = Leaf(a)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  import Tree._

  println(branch(leaf(1), leaf(2)).map(_ + 1))
}


object ContravariantFunctors extends App {

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](f: B => A): Printable[B] = new Printable[B] {
      override def format(b: B): String = self.format(f(b))
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)


  final case class Box[A](value: A)

  //  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
  //    new Printable[Box[A]] {
  //      override def format(box: Box[A]): String = p.format(box.value)
  //    }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap(box => box.value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }

  println(format(Box("hello")))
  println(format(Box(true)))
}

object InvariantFunctors extends App {

  trait Codec[A] { self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(b: B): String = self.encode(enc(b))

      override def decode(s: String): B = dec(self.decode(s))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

  println(encode[Double](1.34))
  println(decode[Double]("1.34"))

  case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap(a => Box(a), box => box.value)

  println(encode[Box[Double]](Box(1.34)))
  println(decode[Box[Double]]("1.34"))
}