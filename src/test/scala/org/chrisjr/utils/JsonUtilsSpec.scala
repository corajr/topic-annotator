package org.chrisjr.utils

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.Random

class JsonUtilsSpec extends FunSpec {
  import JsonUtils._

  describe("The Base-64 utilities") {
    it("should turn an array of floats into a string") {
      Random.setSeed(0L)
      val topics = Array.fill[Float](50)(Random.nextFloat)
      val output = topicsToBase64(topics)
      output.length shouldBe 271
    }
    it("should turn a string back into an array of floats") {
      Random.setSeed(0L)
      val topics = Array.fill[Float](50)(Random.nextFloat)
      val output = topicsToBase64(topics)

      val topics2 = base64ToTopics(output)
      topics shouldEqual topics2

    }
  }

}