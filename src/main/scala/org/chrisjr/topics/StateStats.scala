package org.chrisjr.topics

import scala.collection.mutable

object StateStats {
  val ln2 = math.log(2)
  def log2(d: Double): Double = math.log(d) / ln2
  def entropy(a: Iterable[Double]): Double = {
    val total = a.sum
    val normalized = a.collect { case x if x > 0.0 => x / total }.toArray
    -(normalized.map { x => x * log2(x) }).sum
  }

  /**
   * See Mimno and Blei 2011: https://www.cs.princeton.edu/~blei/papers/MimnoBlei2011.pdf
   *
   * @param state the Gibbs state to process
   * @return a map of topic-words to IMIs
   */
  def getAllImis(state: GibbsState) = {
    val topicEntropies = for {
      topic <- 0 to state.topicsN
    } yield entropy(state.docTopics.map(_._2.apply(topic).toDouble))

    val twByDoc = mutable.HashMap[(Int, Int), Array[Double]]()

    for (Assignment(d, w, t) <- state.assignments) {
      if (!twByDoc.contains((t,w))) twByDoc((t,w)) = Array.fill(state.docsN)(0.0)
      twByDoc((t,w))(d) += 1
    }

    val topicWordImi = SparseMatrix(0.0)

    for (
      topic <- 0 until state.topicsN;
      word <- state.topicTypes(topic).keys;
      entropyDgivenk = topicEntropies(topic);
      entropyDgivenwk = entropy(twByDoc((topic, word)))
    ) {
      if (!entropyDgivenk.isNaN() && !entropyDgivenwk.isNaN())
        topicWordImi(topic)(word) = entropyDgivenk - entropyDgivenwk
    }
    topicWordImi
  }
}