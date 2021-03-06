package org.chrisjr.topic_annotator.utils

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.{ Try, Success, Failure }
import scala.util.Random
import org.chrisjr.topic_annotator.corpora._
import scala.collection.mutable
import play.api.libs.json._
import java.nio.file.{Files, Paths}
import org.chrisjr.topic_annotator.topics._

class JsonUtilsSpec extends FunSpec {
  import JsonUtils._
  import Metadata._

  val metadataMap = mutable.HashMap("title" -> JsString("blah"))
  val sampleMetadata = Metadata(metadataMap.toSeq)
  describe("The Metadata format") {
    it("should be able to serialize document metadata") {
      val out = Json.toJson(sampleMetadata)
    }
    it("should be able to deserialize document metadata") {
      val out = Json.toJson(sampleMetadata).toString
      val metadata = Json.parse(out).asOpt[Metadata]
      metadata should not be empty
      metadata.get shouldBe sampleMetadata
    }
  }

  describe("The Base-64 utilities") {
    it("should turn an array of floats into a string") {
      Random.setSeed(0L)
      val topics = Array.fill[Float](50)(Random.nextFloat)
      val output = topicsToBase64(topics)
      output.length should be >= 268
    }
    it("should turn a string back into an array of floats") {
      Random.setSeed(0L)
      val topics = Array.fill[Float](50)(Random.nextFloat)
      val output = topicsToBase64(topics)

      val topics2 = base64ToTopics(output)
      topics shouldEqual topics2

    }
  }
  
  describe("The Token implicit conversion") {
    val token = Token(start = 1, end = 5, string = "", topic = 1)
    it("should turn a token into JSON") {
      val tokenJSON = Json.toJson(token)
      tokenJSON.toString.size should be > 0
    }
    it("should parse a token from JSON") {
      val tokenJSON = Json.toJson(token)
      val token2 = tokenJSON.as[Token]
      token2 shouldEqual token
    }
  }

}

class VizOutputSpec extends FunSpec with CorpusFixture {
  describe("The Paper Machines output") {
    it("should fail with an unannotated corpus") {
      val dir = Files.createTempDirectory("pm")
      val attempt = Try(JsonUtils.toPaperMachines(corpus, dir.toFile))
      attempt shouldBe 'failure
    }

    it("should work using an annotated corpus") {
      val annotated = MalletLDA.annotate(corpus)
      val dir = Files.createTempDirectory("pm")
      JsonUtils.toPaperMachines(annotated, dir.toFile)
    }
  }
}
