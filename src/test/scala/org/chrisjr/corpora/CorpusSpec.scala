package org.chrisjr.corpora

import org.scalatest._
import java.io.File
import java.io.FileNotFoundException
import scala.util.{ Try, Success, Failure }
import org.scalatest.Matchers._

object CorpusDir {
  val sampleTexts = new File(getClass.getResource("sample-texts").toURI())
}

import CorpusDir._

trait CorpusFixture extends SuiteMixin { this: Suite =>
  val corpus = Corpus.fromDir(sampleTexts).get

  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
  }
}

class CorpusSpec extends FunSpec with CorpusFixture with TryValues {
  val sampleTextsBadEncoding = new File(getClass.getResource("sample-texts-bad-encoding").toURI())

  describe("A Corpus") {
    it("should be empty when initialized") {
      val corpus = Corpus(Seq())
      corpus.documents.size shouldBe 0
    }

    it("should be initializable from a directory") {
      corpus.documents.size should be > 0
    }

    it("should have metadata if a CSV file is available") {
      corpus.documents.map(_.metadata.fields).forall(_.nonEmpty) shouldBe true
    }

    it("should not be initializable from an invalid directory") {
      val corpusTry = Corpus.fromDir(new File("/#!(#$"))
      corpusTry shouldBe 'failure
      corpusTry.failure.exception.getMessage should include("does not exist")
    }

    it("should throw an exception if files are not valid UTF-8") {
      val corpusTry = Corpus.fromDir(sampleTextsBadEncoding)
      corpusTry shouldBe 'failure
    }

    it("should have the same number of documents as text files") {
      val numTextFiles = sampleTexts.listFiles(Corpus.textFilenameFilter).size
      corpus.documents.size shouldBe numTextFiles
    }

    it("should be serializable") {
      val file = File.createTempFile("serializedCorpus", ".tmp")
      Util.pickle(file, corpus)
    }

    it("should be deserializable") {
      val file = File.createTempFile("serializedCorpus", ".tmp")
      Util.pickle(file, corpus)
      val corpus2 = Util.unpickle[Corpus](file)
      corpus shouldEqual corpus2
    }

    it("should be able to apply a series of transformers") {
      val newTransformers = Seq(NoopTransformer, LowercaseTransformer)
      val newCorpus = corpus.transform(newTransformers)
      newCorpus.transformers.size shouldBe newTransformers.size
    }

  }
}