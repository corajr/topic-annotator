package org.chrisjr.topic_annotator.topics

import collection.immutable

import org.chrisjr.topic_annotator.corpora._
import org.chrisjr.topic_annotator.utils.Categorical

class StateAnnotator(val state: GibbsState, val options: TopicModelParams = TopicModelParams.defaultFor(MalletLDA))
  extends CorpusTransformer with PreprocessingTransformer {

  var assignments: immutable.HashMap[java.net.URI, collection.immutable.Vector[Int]] = null

  def preprocess(corpus: Corpus) = {
    def getFreqs(assignments: Seq[Assignment]): Map[Int, Categorical[Int]] = (for {
      (k, a) <- assignments.groupBy(_.word)
      cat = Categorical.fromSeq(a.map(_.topic))
    } yield (k, cat)).toMap

    assignments = immutable.HashMap[java.net.URI, collection.immutable.Vector[Int]]()
    if (options.orderedWords) {
      assignments ++= state.assignments.groupBy(_.doc).map {
        case (i, assignments) =>
          (corpus.documents(i).uri, immutable.Vector() ++ assignments.map(_.topic))
      }
    } else { // assignments were shuffled
      val vocab = corpus.vocab.zipWithIndex.toMap
      val docAssignments = state.assignments.groupBy(_.doc).toSeq
      assignments ++= docAssignments.map {
        case (i, assignments) =>
          val freqs = getFreqs(assignments)
          val doc = corpus.documents(i)
          val words = doc.tokens.map(x => vocab(x.string))
          (doc.uri, immutable.Vector() ++ words.map(i => freqs(i).get))
      }
    }
  }

  def process(doc: Document) = {
    val docAssignments = assignments(doc.uri).iterator
    Seq(doc.copy(tokens = doc.tokens.seq.map { token => token.copy(topic = docAssignments.next) }))
  }
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
