package org.chrisjr.topic_annotator.utils

object Math {
  val ln2 = math.log(2)
  def log2(d: Double): Double = math.log(d) / ln2
  def entropy(a: Iterable[Double]): Double = {
    val total = a.sum
    val normalized = a.collect { case x if x > 0.0 => x / total }
    -(normalized.map { x => x * log2(x) }).sum
  }
}