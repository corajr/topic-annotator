package org.chrisjr.topic_annotator.corpora

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

trait ScoreTransforming { this: FunSpec =>
  def scoring(scoreTransformer: CorpusTransformer, corpus: Corpus) = {
    it("should reduce vocabulary to no more than a specified size") {
      val newCorpus = scoreTransformer(corpus)
      val newScorer = new CorpusScorer(newCorpus)
      newScorer.vocab.size should be <= 10
    }
  }
}

class CorpusTransformerSpec
  extends FunSpec
  with Deserializable
  with ScoreTransforming
  with CorpusFixture
  with CheckTokens
  with TryValues {

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

  describe("A DehyphenationTransformer") {
    it("should combine words split by hyphens") {
      val dehyphenator = DehyphenationTransformer
      val doc = Document.fromString(new java.net.URI(""), "This is a test- ing string").get
      val hyphenCorpus = Corpus(documents = Seq(doc))
      checkTokens(hyphenCorpus, dehyphenator, { x => !x.contains("-") })
    }
  }
  
  describe("A CommonSubstringRemover") {
    it("should remove common substrings longer than a specified length") {
      val csr = new CommonSubstringRemover(minLength = 15)
      val einsteinSample = new File(getClass.getResource("einstein").toURI())
      val csrCorpus = Corpus.fromDir(einsteinSample).get
      checkTokens(csrCorpus, csr, { x => !x.contains("einstein") })
    }
  }

  describe("A ScoreTransformer") {
    import CorpusScorer._

    describe("(with tf-idf scoring)") {
      it should behave like scoring(new ScoreTransformer(topWords = 10, scorerType = TfIdf), corpus)
      it should behave like serializable(new ScoreTransformer(topWords = 10, scorerType = TfIdf), corpus)
    }

    describe("(with log-ent scoring)") {
      it should behave like scoring(new ScoreTransformer(topWords = 10, scorerType = LogEnt), corpus)
      it should behave like serializable(new ScoreTransformer(topWords = 10, scorerType = LogEnt), corpus)
    }
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
