package org.chrisjr.topics

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.chrisjr.corpora.CorpusFixture
import java.io.File
import org.chrisjr.corpora.Corpus

trait AnnotatableState { this: FunSpec =>
  def annotateState(corpus: => Corpus, state: => GibbsState) = {
    it("should match up Gibbs state assignments with corpus tokens") {
      val annotator = new StateAnnotator(state)
      annotator(corpus)
    }
    it("should be able to provide topic labels") {
      val annotator = new StateAnnotator(state)
      val annotated = annotator(corpus)
      
      import StateAnnotator._
      val topN = 5
      val labels = state.topicLabels(annotated.vocab, topN = topN).values
      labels.size shouldEqual (state.topicTypes.keys.max + 1)
      labels.map(_.size).foreach {_ shouldBe topN}
    }
  }
}

class StateAnnotatorSpec extends FunSpec with CorpusFixture with GibbsStateFixture with AnnotatableState {

  describe("A StateAnnotator") {
    describe("(using HDP)") {
      it should behave like annotateState(corpus, hdpState)
    }
    describe("(using MALLET)") {
      it should behave like annotateState(corpus, malletState)
    }
  }
}