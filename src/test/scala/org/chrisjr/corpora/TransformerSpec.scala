package org.chrisjr.corpora

import org.scalatest.FunSpec
import org.scalatest.Matchers._

trait CheckTokens {
  def checkTokens(corpus: Corpus, transformer: CorpusTransformer, f: String => Boolean) = {
    val newCorpus = transformer(corpus)
    for (
      doc <- newCorpus.documents;
      token <- doc.tokens
    ) {
      f(token.string) shouldBe true
    }
  }
}

class CorpusTransformerSpec extends FunSpec with CorpusFixture with CheckTokens {
  describe("A CorpusTransformer") {
    it("should return a new corpus") {
      val transformer = NoopTransformer
      transformer(corpus) should not be null
    }

    it("should be added to the 'transformers' field of the new corpus") {
      val transformer = NoopTransformer
      val newCorpus = transformer(corpus)
      newCorpus.transformers should contain(transformer)
    }
  }

  describe("A LowercaseTransformer") {
    it("should make all tokens lowercase") {
      val lowercaseTransformer = LowercaseTransformer
      checkTokens(corpus, lowercaseTransformer, { x => x == x.toLowerCase })
    }
  }

  describe("A RegexTransformer") {
    it("should transform tokens according to a regex") {
      val regexTransformer = new RegexTransformer("e".r, "i")
      checkTokens(corpus, regexTransformer, { x => !x.contains("e") })
    }
  }

  describe("A TokenFilter") {
    it("should filter tokens according to a predicate") {
      val tokenFilter = new TokenFilter(_.contains("e"))
      checkTokens(corpus, tokenFilter, _.contains("e"))
    }
  }

  describe("A ScoreTransformer") {
    describe("(if topWords is set)") {
      it("should reduce vocabulary to no more than a specified size") {
        val scoreTransformer = new ScoreTransformer(topWords = 10)
        val newCorpus = scoreTransformer(corpus)
        val newScorer = new CorpusScorer(newCorpus)
        newScorer.vocab.size should be <= 10
      }
    }
  }
  
  describe("A sequence of Transformers") {
    they("should compose together") {
      val transformers = Seq(LowercaseTransformer, new RegexTransformer("t".r, ""))
      val aggregate = transformers.reduce(CorpusTransformer.combine)
      checkTokens(corpus, aggregate, {x => (x == x.toLowerCase) && !x.contains("t")})    
    }

    they("should return an altered corpus") {
      val transformers = Seq(LowercaseTransformer, new RegexTransformer("t".r, ""))
      val newCorpus = corpus.transform(transformers)
      checkTokens(newCorpus, NoopTransformer, {x => (x == x.toLowerCase) && !x.contains("t")})    
    }

  }
}