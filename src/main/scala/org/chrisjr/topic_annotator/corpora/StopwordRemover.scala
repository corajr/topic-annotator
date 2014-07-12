package org.chrisjr.topic_annotator.corpora

import scala.util.{Try, Success, Failure}

class StopwordRemover(stopwords: Set[String]) extends TokenFilter({ x => !stopwords.contains(x) })

object StopwordRemover {
  val stoplistDir = new java.io.File(getClass.getResource("stoplists").toURI())
  val stoplistPrefix = "stopwords_"
  def fromFile(file: java.io.File): Try[StopwordRemover] = {
    val source = scala.io.Source.fromFile(file, "UTF-8")
    val stopwords = Try(source.getLines.toSet)
    source.close
    for (stoplist <- stopwords) yield new StopwordRemover(stoplist)
  }
  def forLang(isoCode: String): Try[StopwordRemover] = {
    val stopfile = new java.io.File(stoplistDir, stoplistPrefix + isoCode + ".txt")
    if (stopfile.exists) {
      fromFile(stopfile) 
    } else {
      Failure(new java.io.FileNotFoundException(s"Stoplist for $isoCode not found"))
    }
  }
}
