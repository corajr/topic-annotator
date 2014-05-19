package org.chrisjr.topics

import org.scalatest.FunSpec
import org.chrisjr.corpora.CorpusFixture
import java.io.File
import org.chrisjr.corpora.Corpus

trait AnnotatableState { this: FunSpec =>
  def annotateState(corpus: => Corpus, state: => GibbsState) = {
    it("should match up Gibbs state assignments with corpus tokens") {
      val annotator = new StateAnnotator(state)
      annotator(corpus)
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