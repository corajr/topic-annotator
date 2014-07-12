package org.chrisjr.topic_annotator.corpora

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class TokenSpec extends FunSpec {
  describe("A Token") {
    it("should have a start and end position") {
      val token = Token(0, 1, " ")
      assert(token.start == 0 && token.end == 1)
    }
  }

}
