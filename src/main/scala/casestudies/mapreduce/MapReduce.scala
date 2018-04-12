package casestudies.mapreduce

import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object MapReduce extends App {

  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.foldRight(Monoid[B].empty)((a, b) => f(a) |+| b)


  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val itemsPerChunk = (1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt
    val chunks = values.grouped(itemsPerChunk)
    Future.sequence(chunks.map(chunk => Future(foldMap(chunk)(f)))).map(_.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))

  def parallelFoldMapWithCats[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val itemsPerChunk = (1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt
    values.grouped(itemsPerChunk)
      .toVector
      .traverse(chunk => Future(chunk.foldMap(f)))
      .map(_.combineAll)
  }

  val result2: Future[Int] = parallelFoldMapWithCats((1 to 1000000).toVector)(identity)
  println(Await.result(result2, 1.second))
}
