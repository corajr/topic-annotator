package org.chrisjr.topic_annotator.utils

object Math {
  val ln2 = math.log(2)
  def log2(d: Double): Double = math.log(d) / ln2
  def entropy(a: Iterable[Double]): Double = {
    val total = a.sum
    val normalized = a.collect { case x if x > 0.0 => x / total }
    -(normalized.map { x => x * log2(x) }).sum
  }
  def normalize(xs: Seq[Int]): Array[Double] = {
    val total = xs.sum
    xs.map(_.toDouble / total).toArray
  }
}

class Categorical[T](counts: Seq[(T, Int)]) {
  val (items, weights) = counts.unzip

  val normalized = Math.normalize(weights)
  lazy val map = (items zip normalized).toMap

  val cdf = normalized.scan(0.0)(_ + _).drop(1)

  def get: T = {
    val i = java.util.Arrays.binarySearch(cdf, scala.util.Random.nextDouble)
    if (i < 0) items(-(i + 1)) else items(i)
  }
}
object Categorical {
  def fromSeq[T](items: Seq[T]): Categorical[T] = {
    val countMap = items.groupBy(identity).mapValues(_.size)
    new Categorical(countMap.toSeq)
  }

  def fromMap[T](m: Map[T, Int]) = new Categorical(m.toSeq)
}