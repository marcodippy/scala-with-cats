package monoids

import cats._
import cats.implicits._

object BooleanMonoids {
  implicit val orMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val andMonoid = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val xorMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (y && !x)
  }
}

object SetMonoids {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x | y
  }

  implicit def setIntersectionSemigroup[A] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x & y
  }

  implicit def setDiffMonoid[A] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x &~ y
  }
}

object SuperAdder extends App {
  def add[A](as: List[A])(implicit m: Monoid[A]): A =
    as.foldLeft(m.empty)(_ |+| _)

  println(add(List(1, 2, 3)))
  println(add(List(Some(1), None, Some(3))))

  case class Order(totalCost: Double, quantity: Double)

  implicit def orderMonoid(implicit m: Monoid[Double]) = new Monoid[Order] {
    override def empty: Order = Order(m.empty, m.empty)

    override def combine(x: Order, y: Order): Order = Order(m.combine(x.totalCost, y.totalCost), m.combine(x.quantity, y.quantity))
  }

  println(add(List(Order(0, 0), Order(1, 2), Order(3, 4))))

}