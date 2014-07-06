package org.chrisjr.corpora

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.{ Try, Success, Failure }
import java.io._

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

trait Deserializable { this: FunSpec =>
  def serializable(transformer: CorpusTransformer, corpus: Corpus) = {
    it("should be serializable and deserializable") {
      val ser = File.createTempFile("ser", "ser")
      val transformed = transformer(corpus)
      Util.pickle(ser, transformed)
      Util.unpickle[Corpus](ser)
    }
  }
}

class CorpusTransformerSpec extends FunSpec with Deserializable with CorpusFixture with CheckTokens with TryValues {
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

    it should behave like serializable(LowercaseTransformer, corpus)
  }

  describe("A RegexTransformer") {
    it("should transform tokens according to a regex") {
      val regexTransformer = new RegexTransformer("e".r, "i")
      checkTokens(corpus, regexTransformer, { x => !x.contains("e") })
    }
    it should behave like serializable(new RegexTransformer("e".r, "i"), corpus)
  }

  describe("A TokenFilter") {
    it("should filter tokens according to a predicate") {
      val tokenFilter = new TokenFilter(_.contains("e"))
      checkTokens(corpus, tokenFilter, _.contains("e"))
    }
    it should behave like serializable(new TokenFilter(_.contains("e")), corpus)
  }

  describe("A SnowballTransformer") {
    it("should stem tokens with the full name of a language") {
      val snowballTransformer = new SnowballTransformer("english")
      checkTokens(corpus, snowballTransformer, { x => !x.endsWith("tion") })
    }
    it("should stem tokens with the ISO-639 code of a language") {
      val snowballTransformer = new SnowballTransformer("en")
      checkTokens(corpus, snowballTransformer, { x => !x.endsWith("tion") })
    }
    it("should throw an exception with an invalid language") {
      val transformerTry = Try(new SnowballTransformer("klingon"))
      transformerTry shouldBe 'failure
      transformerTry.failure.exception.getMessage shouldBe "No stemmer found for klingon"
    }
    it should behave like serializable(new SnowballTransformer("english"), corpus)
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
    it should behave like serializable(new ScoreTransformer(topWords = 10), corpus)
  }

  describe("A sequence of Transformers") {
    they("should compose together") {
      val transformers = Seq(LowercaseTransformer, new RegexTransformer("t".r, ""))
      val aggregate = transformers.reduce(CorpusTransformer.combine)
      checkTokens(corpus, aggregate, { x => (x == x.toLowerCase) && !x.contains("t") })
    }

    they("should return an altered corpus") {
      val transformers = Seq(LowercaseTransformer, new RegexTransformer("t".r, ""))
      val newCorpus = corpus.transform(transformers)
      checkTokens(newCorpus, NoopTransformer, { x => (x == x.toLowerCase) && !x.contains("t") })
    }

    they should behave like serializable(
      Seq(LowercaseTransformer, new RegexTransformer("t".r, ""))
        .reduce(CorpusTransformer.combine),
      corpus)

  }
}