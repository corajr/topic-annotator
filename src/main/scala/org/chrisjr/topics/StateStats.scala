package org.chrisjr.topics

import scala.collection.mutable._

class StateStats {
  val ln2 = math.log(2)
  def log2(d: Double): Double = math.log(d) / ln2
  def entropy(a: Array[Double]): Double = {
    val total = a.sum
    val normalized = a.map(_ / total)
    -(normalized.map { x => x * log2(x) }).sum
  }
}