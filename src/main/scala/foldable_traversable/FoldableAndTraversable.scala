package foldable_traversable

import cats._

object Foldable extends App {
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as.foldRight(List.empty[B])((a, bs) => f(a) :: bs)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as.foldRight(List.empty[B])((a, bs) => f(a) ++ bs)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as.foldRight(List.empty[A])((a, acc) => if (f(a)) a :: acc else acc)

  def sum[A](as: List[A])(implicit m: Monoid[A]): A =
    as.foldRight(m.empty)(m.combine)
}
