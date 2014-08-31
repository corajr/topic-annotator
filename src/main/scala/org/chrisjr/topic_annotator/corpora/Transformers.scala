package org.chrisjr.topic_annotator.corpora

import scala.util.matching.Regex
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConversions._
import scala.util.{ Try, Success, Failure }

import CorpusScorer._
import play.api.libs.json.JsString

trait CorpusTransformer extends Serializable {
  def apply(corpus: Corpus) = Corpus(
    corpus.documents.flatMap(process).filter(_.tokens.size > 0),
    corpus.transformers.:+(this))
  def process(document: Document): Seq[Document]
}

object CorpusTransformer {
  //TODO this doesn't work? write a test that illustrates its failure
  def combine(ct1: CorpusTransformer, ct2: CorpusTransformer): CorpusTransformer =
    new CorpusTransformer {
      override def apply(corpus: Corpus) = Corpus(
        corpus.documents.flatMap(process),
        corpus.transformers ++ Seq(ct1, ct2))
      def process(document: Document) = ct1.process(document).flatMap(ct2.process)
    }
}

class TokenTransformer(f: String => String) extends CorpusTransformer {
  def process(doc: Document) = Seq(doc.copy(tokens =
    doc.tokens.map { token => token.copy(string = f(token.string)) }))
}

class TokenFilter(f: String => Boolean) extends CorpusTransformer {
  def process(doc: Document) = Seq(doc.copy(tokens = doc.tokens.filter { token => f(token.string) }))
}

trait DocumentFilter extends CorpusTransformer {
  def pred(doc: Document): Boolean
  def process(doc: Document): Seq[Document] = if (pred(doc)) Seq(doc) else Seq()
}

class DocumentFilterBy(f: Document => Boolean) extends DocumentFilter {
  def pred(doc: Document) = f(doc)
}

object NoopTransformer extends CorpusTransformer { def process(doc: Document) = Seq(doc) }

object LowercaseTransformer extends TokenTransformer(_.toLowerCase)

object DehyphenationTransformer extends CorpusTransformer {
  def process(doc: Document) = {
    val newTokens = collection.mutable.ArrayBuffer[Token]()
    val tokenIterator = doc.tokens.iterator
    while (tokenIterator.hasNext) {
      val token = tokenIterator.next
      if (token.string.endsWith("-") && tokenIterator.hasNext) {
        val token2 = tokenIterator.next
        val newString = token.string.substring(0, token.string.length - 1) + token2.string
        newTokens += Token(start = token.start, end = token2.end, string = newString)
      } else {
        newTokens += token
      }
    }
    Seq(doc.copy(tokens = newTokens))
  }
}

object Snowball {
  import org.tartarus.snowball._

  // these are all the snowball languages available
  val iso639 = Map("ru" -> "russian",
    "fr" -> "french",
    "en" -> "english",
    "pt" -> "portuguese",
    "no" -> "norwegian",
    "sv" -> "swedish",
    "de" -> "german",
    "tr" -> "turkish",
    "it" -> "italian",
    "da" -> "danish",
    "fi" -> "finnish",
    "hu" -> "hungarian",
    "es" -> "spanish",
    "nl" -> "dutch")

  class LocalStemmer(language: String) extends Serializable {
    @transient lazy val stemmer: SnowballStemmer = getSnowballStemmer(language)
    def stem(word: String) = {
      stemmer.setCurrent(word)
      stemmer.stem()
      stemmer.getCurrent()
    }
  }

  def getSnowballStemmer(language: String): SnowballStemmer = {
    val lang = if (iso639.contains(language)) iso639(language) else language
    val findClass = Try(Class.forName("org.tartarus.snowball.ext." + lang + "Stemmer"))
    findClass match {
      case Success(stemClass) =>
        stemClass.newInstance().asInstanceOf[SnowballStemmer]
      case Failure(e) =>
        throw new ClassNotFoundException(s"No stemmer found for $language")
    }
  }

  def getStemFuncFor(lang: String) = {
    val stemmer = new LocalStemmer(lang)

    if (!iso639.contains(lang) && !iso639.values.contains(lang))
      throw new ClassNotFoundException(s"No stemmer found for $lang")
    stemmer.stem _
  }
}

class SnowballTransformer(lang: String) extends TokenTransformer(Snowball.getStemFuncFor(lang))

class RegexTransformer(regex: Regex, sub: String) extends TokenTransformer({ x =>
  regex.replaceAllIn(x, sub)
})

class MinLengthRemover(minLength: Int) extends TokenFilter({ x => x.length >= minLength })

trait PreprocessingTransformer extends CorpusTransformer {
  def preprocess(corpus: Corpus): Unit
  abstract override def apply(corpus: Corpus) = {
    preprocess(corpus)
    super.apply(corpus)
  }
}

class ScoreTransformer(topWords: Int = 5000, minDf: Int = 3, scorerType: ScorerType = TfIdf)
  extends CorpusTransformer with PreprocessingTransformer {
  val stopwords = new ConcurrentSkipListSet[String]

  def medianTransform(ascending: Iterable[(String, Double)], n: Int): Iterable[(String, Double)] = {
    val median = ascending.size / 2
    val start = math.max(median - n / 2, 0)
    val end = math.min(median + n / 2, ascending.size - 1)
    ascending.slice(start, end) ++ ascending.take(start) ++ ascending.drop(end)
  }

  def preprocess(corpus: Corpus) = {
    val scorer = new CorpusScorer(corpus, minDf)
    val stops = (scorerType match {
      case MinDf =>
        val dfs = scorer.dfScore.toSeq.sortBy(_._2).reverse
        if (topWords < Int.MaxValue) dfs.drop(topWords).unzip._1
        else dfs.dropWhile(_._2 >= minDf).unzip._1
      case TfIdf =>
        scorer.tfidf.seq.toSeq.sortBy(_._2).reverse.drop(topWords).unzip._1
      case LogEnt =>
        //        medianTransform(scorer.logent.sortBy(_._2), topWords)
        scorer.logent.sortBy(_._2).reverse.drop(topWords).unzip._1
    })

    stopwords.addAll(stops)
  }

  def process(doc: Document) = Seq(doc.copy(tokens =
    doc.tokens.filter { token => !stopwords.contains(token.string) }))
}

class Dedupe extends PreprocessingTransformer with DocumentFilter {
  var toKeep = collection.immutable.HashSet[Int]()

  def preprocess(corpus: Corpus) = {
    corpus.documents.groupBy(_.text.hashCode).values.foreach { x =>
      toKeep += x.head.hashCode
    }
  }

  def pred(doc: Document) = toKeep.contains(doc.hashCode)
}

class CommonSubstringRemover(minLength: Int = 20, grouping: Document => String = { _ => "" }, maxLength: Int = 10000)
  extends CorpusTransformer with PreprocessingTransformer {
  var excludedRanges = collection.immutable.HashMap[java.net.URI, Vector[(Int, Int)]]()

  import com.googlecode.concurrenttrees.solver.LCSubstringSolver
  import com.googlecode.concurrenttrees.radix.node.concrete.DefaultCharSequenceNodeFactory
  import com.googlecode.concurrenttrees.common.CharSequences

  def preprocess(corpus: Corpus) = {
    var i = 0
    val groups = corpus.documents.groupBy(grouping)
    for (
      (key, group) <- groups if group.size > 1
    ) {
      val solver = new LCSubstringSolver(new DefaultCharSequenceNodeFactory)
      for (doc <- group.take(2)) {
        val text = doc.text
        if (text.length > 0) solver.add(text.substring(0, math.min(text.length, maxLength)))
      }

      val lcs = CharSequences.toString(solver.getLongestCommonSubstring())

      i += group.size

      if (lcs.length >= minLength) {
        val regex = java.util.regex.Pattern.quote(lcs).r
        for (doc <- group) {
          excludedRanges += doc.uri -> regex.findAllMatchIn(doc.text).map { x => (x.start, x.end) }.toVector
        }
        //        println(s"$i done, lcs for group: ${lcs.length()}")
      }
    }
  }

  def process(doc: Document) = {
    val ranges = excludedRanges.getOrElse(doc.uri, Seq())
    Seq(doc.copy(tokens = doc.tokens.filter { x =>
      !ranges.exists { r => x.start >= r._1 && x.end <= r._2 }
    }))
  }
}

class DmrFeatures(features: Set[String] = Set("time", "journal")) extends PreprocessingTransformer {
  type FeatureMap = Map[String, Either[Double, String]]
  var allFeatures = collection.immutable.HashMap[java.net.URI, String]()

  // abuse of Either
  def mapToString(m: FeatureMap): String = (for {
    (k, value) <- m
    res = value match {
      case Left(n) => "=" + n.toString
      case Right(s) => s.replaceAll("\\W+", "")
    }
  } yield s"$k$res").mkString(" ")

  def getYear(doc: Document): Option[Int] = (doc.metadata \ "issued" \ "date-parts")(0)(0).asOpt[String].map(_.toInt)
  def getJournal(doc: Document) = (doc.metadata \ "container-title").asOpt[String].getOrElse("")

  def preprocess(corpus: Corpus) = {
    val years = if (features.contains("time")) corpus.documents.flatMap(getYear).seq.sorted else Seq()

    for (doc <- corpus.documents) {
      val timeMap: FeatureMap = if (features.contains("time")) {
        val pd = (getYear(doc).getOrElse(years.head).toDouble - years.head) / (years.last - years.head)
        Map("pd" -> Left(pd), "oneminuspd" -> Left(1.0 - pd))
      } else Map()

      val journalMap: FeatureMap = if (features.contains("journal")) {
        Map("journal" -> Right(getJournal(doc)))
      } else Map()

      val fs = timeMap ++ journalMap
      allFeatures += doc.uri -> mapToString(fs)
    }
  }
  def process(doc: Document) = Seq(doc.copy(metadata = doc.metadata + ("features" -> JsString(allFeatures(doc.uri)))))
}