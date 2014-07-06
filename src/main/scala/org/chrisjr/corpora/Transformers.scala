package org.chrisjr.corpora

import scala.util.matching.Regex
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConversions._

import scala.util.{ Try, Success, Failure }

trait CorpusTransformer extends Serializable {
  def apply(corpus: Corpus) = Corpus(corpus.documents.map(process), corpus.transformers.:+(this))
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

object Snowball {
  import org.tartarus.snowball._

  val stemmers = collection.mutable.Map[String, Option[SnowballStemmer]]()

  def getStemmerFor(lang: String): Option[SnowballStemmer] = {
    stemmers.getOrElseUpdate(lang, {
      val findClass = Try(Class.forName("org.tartarus.snowball.ext." + lang + "Stemmer"))
      findClass match {
        case Success(stemClass) =>
          Some(stemClass.newInstance().asInstanceOf[SnowballStemmer])
        case Failure(e) =>
          None
      }
    })
  }

  def mkStemFunc(stemmer: SnowballStemmer) = {
    { original: String =>
      stemmer.setCurrent(original)
      stemmer.stem()
      stemmer.getCurrent()
    }
  }

  def getStemFuncFor(lang: String) = {
    val stemmerOpt = getStemmerFor(lang)
    if (stemmerOpt.isEmpty) throw new ClassNotFoundException(s"No stemmer found for $lang")
    mkStemFunc(stemmerOpt.get)
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

class ScoreTransformer(topWords: Int = 5000, minDf: Int = 3)
  extends CorpusTransformer with PreprocessingTransformer {
  val stopwords = new ConcurrentSkipListSet[String]

  def preprocess(corpus: Corpus) = {
    val scorer = new CorpusScorer(corpus, minDf)
    val scores = scorer.tfidf.seq.toSeq.sortBy(_._2).reverse
    stopwords.addAll(scores.drop(topWords).unzip._1)
  }

  def process(doc: Document) = doc.copy(tokens =
    doc.tokens.filter { token => !stopwords.contains(token.string) })
}