package org.chrisjr.topic_annotator.corpora

import org.scalatest._
import org.scalatest.Matchers._
import java.io.File

//import scala.util.{Try, Success, Failure}

class StopwordRemoverSpec extends FunSpec with CorpusFixture with TryValues with CheckTokens with Deserializable {
  val stoplistDir = StopwordRemover.stoplistDir

  describe("A StopwordRemover") {
    it("should eliminate 'a', 'the', 'an', 'of'") {
      val commonWords = Set("a", "the", "an", "of")
      val stoplistEnglish = StopwordRemover.forLang("en").get
      checkTokens(corpus, stoplistEnglish, { x => !commonWords.contains(x) })
    }

    describe("(by ISO 639-1 code)") {
      it("should exist for English") {
        StopwordRemover.forLang("en") shouldBe 'success
      }
      it("should throw an exception on an invalid code") {
        StopwordRemover.forLang("zz") shouldBe 'failure
      }
      it should behave like serializable(StopwordRemover.forLang("en").get, corpus)
    }
  }
}
