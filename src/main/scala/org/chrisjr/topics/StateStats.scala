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
    def twByDoc(state: GibbsState, topic: Int, word: Int) = {
      val perDoc = Array.fill(state.docsN)(0.0)
      for (Assignment(d, w, t) <- state.assignments if t == topic && w == word) {
        perDoc(d) += 1
      }
      perDoc
    }

    val topicEntropies = for {
      topic <- 0 to state.topicsN
    } yield entropy(state.docTopics.map(_._2.apply(topic).toDouble))

    val topicWordImi = SparseMatrix(0.0)

    for (
      topic <- 0 until state.topicsN;
      word <- state.topicTypes(topic).keys;
      entropyDgivenk = topicEntropies(topic);
      entropyDgivenwk = entropy(twByDoc(state, topic, word))
    ) {
      if (!entropyDgivenk.isNaN() && !entropyDgivenwk.isNaN())
        topicWordImi(topic)(word) = entropyDgivenk - entropyDgivenwk
    }
    topicWordImi
  }
}