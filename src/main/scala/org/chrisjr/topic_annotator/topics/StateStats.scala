package org.chrisjr.topic_annotator.topics

import org.chrisjr.topic_annotator.utils.Math._

import scala.collection.mutable

object StateStats {

  /**
   * @param ps the probabilities of P as an array
   * @param q the probabilities of Q as an array
   * @return sqrt of the Jensen-Shannon Divergence, a metric with values in [0, 1]
   */
  def jsdMetric(p: Iterable[Double], q: Iterable[Double]) = {
    val m = (p zip q).map { case (pi, qi) => 0.5 * (pi + qi) }
    math.sqrt(entropy(m) - (0.5 * (entropy(p) + entropy(q))))
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
      if (!twByDoc.contains((t, w))) twByDoc((t, w)) = Array.fill(state.docsN)(0.0)
      twByDoc((t, w))(d) += 1
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
