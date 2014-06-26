package org.chrisjr.topics

//import scala.util.parsing.combinator._
import java.io._
import scala.io.Source
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.util.parsing.input.StreamReader

case class Assignment(doc: Int, word: Int, topic: Int)

class SparseMatrix[V: Numeric] extends mutable.HashMap[Int, mutable.Map[Int, V]] with Serializable {
  var defaultV = implicitly[Numeric[V]].zero
  override def apply(x: Int) = this.getOrElseUpdate(x, mutable.Map[Int, V]().withDefaultValue(defaultV))
  def normalized = {
    this.map {
      case (i, m) =>
        val total = implicitly[Numeric[V]].toDouble(m.values.sum)
        i -> m.mapValues(v => implicitly[Numeric[V]].toDouble(v) / total)
    }.toMap
  }
}

object SparseMatrix {
  def apply[V: Numeric](v: V): SparseMatrix[V] = {
    val x = new SparseMatrix[V]()
    x.defaultV = v
    x
  }
}

class GibbsState extends Serializable {
  val docTopics = SparseMatrix(0)
  val topicTypes = SparseMatrix(0)
  val assignments = mutable.ArrayBuffer[Assignment]()
  var topicsN = 0
  var wordsN = 0
  var docsN = 0

  def add(a: Assignment) = {
    assignments += a
    docTopics(a.doc)(a.topic) += 1
    topicTypes(a.topic)(a.word) += 1
    if (a.topic == topicsN) topicsN = a.topic + 1
    if (a.word == wordsN) wordsN = a.word + 1
    if (a.doc == docsN) docsN = a.doc + 1
  }
}

trait GibbsStateReader extends GibbsStateParser {
  def fromInputStream(is: InputStream): GibbsState = {
    val state = new GibbsState
    val source = Source.fromInputStream(is)
    for (a <- parse(source.getLines)) {
      state.add(a)
    }
    source.close
    state
  }
  def fromFile(file: File): GibbsState = fromInputStream(new FileInputStream(file))
}

/*
trait GibbsStateParser extends RegexParsers with JavaTokenParsers {
  def int = wholeNumber ^^ { _.toInt }
  def string = """\w+""".r
  def assignment: Parser[Assignment]
  def assignments = assignment*
}

trait MalletStateParser extends GibbsStateParser {
  def assignment = int ~ (string ~> wholeNumber ~> int) ~ (string ~> int) ^^ { case doc ~ word ~ topic => Assignment(doc, word, topic) }
}

trait HDPStateParser extends GibbsStateParser {
  def assignment = int ~ int ~ (int ~> int) ^^ { case doc ~ word ~ topic => Assignment(doc, word, topic) }
}
*/

trait GibbsStateParser {
  def parseLine: PartialFunction[Array[String], Assignment]
  def parse(lines: Iterator[String]): Iterator[Assignment] = {
    lines.map(_.split(" ")).collect(parseLine)
  }
}

trait MalletStateParser extends GibbsStateParser {
  def parseLine = { case Array(doc, _, _, word, _, topic) if doc.forall(_.isDigit) => Assignment(doc.toInt, word.toInt, topic.toInt) }
}
trait HDPStateParser extends GibbsStateParser {
  def parseLine = { case Array(doc, word, _, topic) if doc.forall(_.isDigit) => Assignment(doc.toInt, word.toInt, topic.toInt) }
}

object MalletStateReader extends GibbsStateReader with MalletStateParser {
  override def fromFile(file: File) = fromInputStream(new GZIPInputStream(new FileInputStream(file)))
}

object HDPStateReader extends GibbsStateReader with HDPStateParser
  