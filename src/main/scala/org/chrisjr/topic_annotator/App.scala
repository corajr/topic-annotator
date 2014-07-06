package org.chrisjr.topic_annotator

import java.io.File
import org.chrisjr.corpora._
import scala.util.{ Try, Success, Failure }
import org.chrisjr.topics._
import org.chrisjr.utils.JsonUtils

/**
 * @author ${user.name}
 */
object App {

  def main(args: Array[String]) {
    importCorpusAndProcess(args)
    /*    
    val startTime = System.currentTimeMillis()
    val state = MalletStateReader.fromFile(new File("/Users/chrisjr/Desktop/desk/viz_ethno/lda/topic-state.gz"))
    val imis = StateStats.getAllImis(state)

    println(s"${(System.currentTimeMillis() - startTime) / 1000.0} seconds.")
    */
  }

  def importCorpusAndProcess(args: Array[String]) = {
    val inputDirOpt = if (args.length > 0) Some(new File(args(0))) else None
    val inputDir = inputDirOpt.getOrElse(new File(Class.forName("org.chrisjr.corpora.Corpus").getResource("sample-texts").toURI))
    val outputCorpusFile = if (args.length > 1) Some(new File(args(1))) else None

    var corpus: Corpus = null

    if (outputCorpusFile.isEmpty || outputCorpusFile.forall(!_.exists)) {
      var startTime = System.currentTimeMillis()
      var docsN = 0
      val corpusTry = Corpus.fromDir(inputDir)
      corpusTry match {
        case Success(c) =>
          docsN = c.documents.size; println(s"$docsN documents loaded")
        case Failure(e) => e.printStackTrace
      }

      val transformers = Seq(LowercaseTransformer,
        new MinLengthRemover(4),
        StopwordRemover.forLang("en").get,
        StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/stopwords.txt")).get,
        StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/author_names.txt")).get,
        new SnowballTransformer("english"),
        new ScoreTransformer(topWords = 5000))

      corpus = corpusTry.get.transform(transformers)
      val elapsedTime = System.currentTimeMillis() - startTime
      println(s"Took ${elapsedTime / 1000.0} seconds (avg. ${elapsedTime.toFloat / docsN} ms per doc).")

      outputCorpusFile foreach { f => Util.pickle(f, corpus) }
    } else {
      corpus = Util.unpickle[Corpus](outputCorpusFile.get)
    }

    val annotatedFile = new File(outputCorpusFile.get.getPath + ".mallet")

    val annotated = if (!annotatedFile.exists) {
//      val options = TopicModelParams.defaultFor(MalletLDA)
//      options.numTopics = 50
//      val annotatedCorpus = MalletLDA.annotate(corpus, options)
      val options = TopicModelParams.defaultFor(HDP)
      val annotatedCorpus = HDP.annotate(corpus, options)
      Util.pickle(annotatedFile, annotatedCorpus)
      annotatedCorpus
    } else {
      Util.unpickle[Corpus](annotatedFile)
    }

    val outDir = outputCorpusFile.get.getParentFile
    JsonUtils.toPaperMachines(annotated, new File("/Users/chrisjr/Desktop/success"))
  }
}
