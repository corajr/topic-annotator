package org.chrisjr.utils

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.Random
import org.chrisjr.corpora._
import scala.collection.immutable
import play.api.libs.json._

class JsonUtilsSpec extends FunSpec {
  import JsonUtils._

  val metadataMap = immutable.HashMap("title" -> JsString("blah"))
  val sampleMetadata = Metadata(metadataMap)
  describe("The Metadata format") {
    it("should be able to serialize document metadata") {
      val out = Json.toJson(sampleMetadata)
    }
    it("should be able to deserialize document metadata") {
      val out = Json.toJson(sampleMetadata)
      val metadata = Json.fromJson[Metadata](out).asOpt
      metadata should not be empty
      metadata.get shouldBe sampleMetadata
    }
  }

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