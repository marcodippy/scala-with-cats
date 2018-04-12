package typeclasses

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def print[A](a: A)(implicit p: Printable[A]): Unit = println(format(a))
}

object PrintableSyntax {

  implicit class PrintableOps[A](a: A) {
    def format(implicit p: Printable[A]): String = p.format(a)

    def print(implicit p: Printable[A]): Unit = println(p.format(a))
  }

}

final case class Cat(name: String, age: Int, color: String)

object Test extends App {

  val tomCat = Cat("Tom", 9, "Grey")
  import PrintableInstances._

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      override def format(a: Cat): String =
        s"${Printable.format(a.name)} is a ${Printable.format(a.age)} year-old ${Printable.format(a.color)} cat."
    }

  Printable.print(tomCat)

  import PrintableSyntax._
  tomCat.print

}


object WithCats extends App {
  import cats._
  import cats.implicits._

  implicit val catShow: Show[Cat] = Show.show(cat => {
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."
  })

  println(Cat("Tom", 9, "Grey").show)
}