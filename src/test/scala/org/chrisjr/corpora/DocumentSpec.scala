package org.chrisjr.corpora

import org.scalatest.FunSpec
import org.scalatest.TryValues
import org.scalatest.Matchers._
import java.io.File
import java.io.FileNotFoundException
import scala.util.{Try, Success, Failure}

class DocumentSpec extends FunSpec with TryValues {
  val sampleTexts = new File(getClass.getResource("sample-texts").toURI())
  val sampleTextsBadEncoding = new File(getClass.getResource("sample-texts-bad-encoding").toURI())

  val validUtf8file = new File(sampleTexts, "innocents-abroad-1.txt")
  val invalidUtf8file = new File(sampleTextsBadEncoding, "innocents-abroad-1.txt")

  describe("A Document") {
    it("should be empty to start") {
      val doc = Document("", Seq())
      doc.tokens.size shouldBe 0
    }
    
    it("should be initializable from a string") {
      val docTry = Document.fromString("test1", "This is a test.")
      docTry shouldBe 'success
    }
    
    it("should be initializable from a UTF-8 text file") {
      val docTry = Document.fromTextFile(validUtf8file)
      docTry shouldBe 'success      
    }

    it("should not be initializable from a text file of unknown encoding") {
      val docTry = Document.fromTextFile(invalidUtf8file)
      docTry shouldBe 'failure    
    }

    it("should have tokens that correspond to their string offsets") {
      val source = scala.io.Source.fromFile(validUtf8file)
      val text = source.getLines.mkString("\n")
      source.close

      val doc = Document.fromTextFile(validUtf8file).get
      for (token <- doc.tokens) {
        val tokenString = text.substring(token.start, token.end)
        tokenString shouldBe token.string
      }
    }
    
    it("should be serializable") {
      val doc = Document.fromTextFile(validUtf8file).get
      val file = File.createTempFile("serializedDoc", ".tmp")
      Util.pickle(file, doc)
    }

    it("should be deserializable") {
      val doc = Document.fromTextFile(validUtf8file).get
      val file = File.createTempFile("serializedDoc", ".tmp")
      Util.pickle(file, doc)
      val doc2 = Util.unpickle[Document](file)
      doc shouldEqual doc2
    }
    
    it("should allow retrieving annotations as HTML") {
      val doc = Document.fromTextFile(validUtf8file).get
      println(doc.topicsHTML)
      doc.topicsHTML should include ("</span>")
    }
  }
}