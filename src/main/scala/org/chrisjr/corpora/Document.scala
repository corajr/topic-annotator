package org.chrisjr.corpora

import scala.io.Source
import java.io._
import java.net.URI
import scala.collection.mutable
import scala.collection.GenSeq
import scala.util.matching.Regex
import scala.util.{ Try, Success, Failure }
import java.nio.charset.Charset

import MetadataCollection._

import Metadata._
case class Document(uri: URI, metadata: Metadata = noMetadata, tokens: GenSeq[Token]) {
  def topicsHTML = {
    val reader = new BufferedReader(
      new InputStreamReader(
        new FileInputStream(
          new File(uri)), Charset.forName("UTF-8")))
    var c = reader.read()
    var i = 0
    val tokenIterator = tokens.iterator
    var token = tokenIterator.next

    val sb = new StringBuilder
    while (c != -1) {
      if (i == token.start) {
        sb ++= s"<span class='topic${token.topic}'>"
      } else if (i == token.end) {
        sb ++= "</span>"
        if (tokenIterator.hasNext) token = tokenIterator.next
      }

      sb += c.toChar

      i += 1
      c = reader.read()
    }

    if (i == token.end) {
      sb ++= "</span>"
      if (tokenIterator.hasNext) token = tokenIterator.next
    }

    reader.close
    sb.toString
  }
}

object Document {
  def tokensFromLineIterator(lines: Iterator[String], regex: Regex = "\\p{Alpha}+".r) = {
    var offset = 0
    val tokens = mutable.ArrayBuffer[Token]()
    while (lines.hasNext) {
      val line = lines.next
      tokens ++= regex.findAllMatchIn(line).map { x => Token(x.start + offset, x.end + offset, x.matched, -1) }
      offset += line.length + 1
    }
    tokens
  }

  def fromTextFile(file: File, encoding: String = "UTF-8", metadata: Metadata = noMetadata): Try[Document] = {
    val source = Source.fromFile(file, encoding)
    val docTry = Try(Document(uri = file.toURI(), metadata = metadata, tokens = tokensFromLineIterator(source.getLines)))
    source.close
    docTry
  }

  def fromString(uri: URI, text: String, metadata: Metadata = noMetadata) = {
    Success(Document(uri = uri, metadata = metadata, tokens = tokensFromLineIterator(text.split("\n").iterator)))
  }
}