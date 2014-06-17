package org.chrisjr.topic_annotator

import java.io.File
import org.chrisjr.corpora._
import scala.util.{ Try, Success, Failure }
import org.chrisjr.topics._

/**
 * @author ${user.name}
 */
object App {

  def main(args: Array[String]) {
    val inputDirOpt = if (args.length > 0) Some(new File(args(0))) else None
    val inputDir = inputDirOpt.getOrElse(new File(Class.forName("org.chrisjr.corpora.Corpus").getResource("sample-texts").toURI))
    val outputCorpusFile = if (args.length > 1) Some(new File(args(1))) else None

    var corpus: Corpus = null

    if (outputCorpusFile.isEmpty || !outputCorpusFile.forall(_.exists)) {
      var startTime = System.currentTimeMillis()
      var docsN = 0
      val corpusTry = Corpus.fromDir(inputDir)
      corpusTry match {
        case Success(c) =>
          docsN = c.documents.size; println(s"$docsN documents loaded")
        case Failure(e) => e.printStackTrace
      }

      val transformers = Seq(LowercaseTransformer,
        StopwordRemover.forLang("en").get,
        new ScoreTransformer())

      corpus = corpusTry.get.transform(transformers)
      val elapsedTime = System.currentTimeMillis() - startTime
      println(s"Took ${elapsedTime / 1000.0} seconds (avg. ${elapsedTime.toFloat / docsN} ms per doc).")

      outputCorpusFile foreach { f => Util.pickle(f, corpus) }
    } else {
      corpus = Util.unpickle[Corpus](outputCorpusFile.get)
    }

    val annotated = HDP.annotate(corpus)
    
    Util.pickle(new File(outputCorpusFile.get.getPath + ".hdp"), annotated)

    /*
    for (doc <- annotated.documents) {
      println(doc.topicsHTML)
    }
    */
  }
}
