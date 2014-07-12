package org.chrisjr.topic_annotator.corpora

import org.scalatest._
import org.scalatest.Matchers._

trait CorpusScorerFixture extends SuiteMixin with CorpusFixture { this: Suite =>
  val scorer = new CorpusScorer(corpus, minDf = 0)
  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
  }
}

class CorpusScorerSpec extends FunSpec with CorpusScorerFixture {
  describe("A CorpusScorer") {
    describe("returns a vocabulary that") {
      it("should be a map") {
        scorer.vocab should not be null
      }
      it("should be comprehensive") {
        val vocab = scorer.vocab
        val localVocab = (for {
          doc <- corpus.documents;
          token <- doc.tokens
          tokenStr = token.string
        } yield tokenStr).toSet
        vocab.size shouldBe localVocab.size
      }
    }

    it("should return TF-IDF scores") {
      scorer.tfidf.size should be > 0
    }
  }

}
