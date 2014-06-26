package org.chrisjr.topics

import org.chrisjr.corpora._

class StateAnnotator(val state: GibbsState) extends CorpusTransformer {
  val assignments = state.assignments.map(_.topic).iterator

  def process(doc: Document) = doc.copy(tokens = doc.tokens.map { token => token.copy(topic = assignments.next) })
}

object StateAnnotator {
  implicit class StateWithVocab(state: GibbsState) {
    lazy val dt = state.docTopics.normalized
    lazy val tw = state.topicTypes.normalized

    def topicLabels(implicit vocab: Seq[String], topN: Int = 5): Map[Int, Seq[String]] = {
      (for {
        i <- 0 to tw.keys.max;
        label = tw(i).toSeq.sortBy(_._2).reverse.take(topN).unzip._1.map(vocab)
      } yield i -> label).toMap
    }
  }
}