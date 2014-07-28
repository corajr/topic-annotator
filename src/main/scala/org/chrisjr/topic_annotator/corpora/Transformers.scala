package org.chrisjr.topic_annotator.corpora

import scala.util.matching.Regex
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConversions._

import scala.util.{ Try, Success, Failure }

trait CorpusTransformer extends Serializable {
  def apply(corpus: Corpus) = Corpus(
    corpus.documents.map(process).filter(_.tokens.size > 0),
    corpus.transformers.:+(this))
  def process(document: Document): Document
}

object CorpusTransformer {
  //TODO this doesn't work? write a test that illustrates its failure
  def combine(ct1: CorpusTransformer, ct2: CorpusTransformer): CorpusTransformer =
    new CorpusTransformer {
      override def apply(corpus: Corpus) = Corpus(
        corpus.documents.map(process),
        corpus.transformers ++ Seq(ct1, ct2))
      def process(document: Document) = ct2.process(ct1.process(document))
    }
}

class TokenTransformer(f: String => String) extends CorpusTransformer {
  def process(doc: Document) = doc.copy(tokens =
    doc.tokens.map { token => token.copy(string = f(token.string)) })
}

class TokenFilter(f: String => Boolean) extends CorpusTransformer {
  def process(doc: Document) = doc.copy(tokens = doc.tokens.filter { token => f(token.string) })
}

object NoopTransformer extends CorpusTransformer { def process(doc: Document) = doc }

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
    doc.copy(tokens = newTokens)
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

import CorpusScorer._

class ScoreTransformer(topWords: Int = 5000, minDf: Int = 3, scorerType: ScorerType = TfIdf)
  extends CorpusTransformer with PreprocessingTransformer {
  val stopwords = new ConcurrentSkipListSet[String]

  def preprocess(corpus: Corpus) = {
    val scorer = new CorpusScorer(corpus, minDf)
    val scores = (scorerType match {
      case TfIdf =>
        scorer.tfidf
      case LogEnt =>
        scorer.logent
    }).seq.toSeq.sortBy(_._2).reverse

    stopwords.addAll(scores.drop(topWords).unzip._1)
  }

  def process(doc: Document) = doc.copy(tokens =
    doc.tokens.filter { token => !stopwords.contains(token.string) })
}
