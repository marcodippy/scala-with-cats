package casestudies.crdts

import cats._
import cats.kernel.CommutativeMonoid
import cats.implicits._
import cats.kernel.BoundedSemilattice

object GCounterV1 {

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] =
      GCounter(counters.updated(machine, counters.getOrElse(machine, m.empty) |+| amount))

    def merge(that: GCounter[A])(implicit s: BoundedSemilattice[A]): GCounter[A] =
      GCounter(counters |+| that.counters)


    def total(implicit m: CommutativeMonoid[A]): A =
      counters.values.toList.combineAll
  }

}

object TypeClassInstances {

  implicit val intBoundedSemilattice: BoundedSemilattice[Int] = new BoundedSemilattice[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x max y
  }

  implicit def setBoundedSemilattice[A]: BoundedSemilattice[Set[A]] =
    new BoundedSemilattice[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    }
}


trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemilattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] =
    counter

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] =
      f.updated(k, f.getOrElse(k, m.empty) |+| v)

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemilattice[V]): Map[K, V] =
      f1 |+| f2

    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.toList.combineAll
  }
}

object Test extends App {

  import TypeClassInstances._

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  println(counter.total(merged)(intBoundedSemilattice))
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  def mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

}

object Test2 extends App {

  import KeyValueStore._

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(implicit m: Monoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemilattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }

}