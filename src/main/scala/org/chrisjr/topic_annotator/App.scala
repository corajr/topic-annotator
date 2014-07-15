package org.chrisjr.topic_annotator

import java.io.File
import org.chrisjr.topic_annotator.corpora._
import scala.util.{ Try, Success, Failure }
import org.chrisjr.topic_annotator.topics._
import org.chrisjr.topic_annotator.utils.JsonUtils

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
    val inputDir = inputDirOpt.getOrElse(new File(Class.forName("org.chrisjr.topic_annotator.corpora.Corpus").getResource("sample-texts").toURI))
    val corpusFile = if (args.length > 1) Some(new File(args(1))) else None
    val preprocessedCorpusFile = if (args.length > 2) Some(new File(args(2))) else None
    val annotatedFile = new File(preprocessedCorpusFile.get.getPath + ".mallet")

    def doOrUnpickle[T <: Serializable](fileOpt: Option[File], block: => T): T = {
      if (fileOpt.nonEmpty && fileOpt.get.exists) Util.unpickle[T](fileOpt.get)
      else {
        val item = block
        if (fileOpt.nonEmpty) Util.pickle(fileOpt.get, item)
        item
      }
    }

    var startTime = System.currentTimeMillis()
    var docsN = 0

    val annotated = doOrUnpickle(Some(annotatedFile), {
      val preprocessed = doOrUnpickle(preprocessedCorpusFile, {
        val raw = doOrUnpickle(corpusFile, {
          val corpusTry = Corpus.fromDir(inputDir)
          val myCorpus = corpusTry.get
          myCorpus
        })
        docsN = raw.documents.size
        println(s"$docsN documents loaded")

        val transformers = Seq(LowercaseTransformer,
          new MinLengthRemover(4),
          StopwordRemover.forLang("en").get,
          StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/stopwords.txt")).get,
          StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/author_names.txt")).get,
          new SnowballTransformer("english"),
          new ScoreTransformer(topWords = 10000, minDf = 10))
        raw.transform(transformers)
      })

      val elapsedTime = System.currentTimeMillis() - startTime
      println(s"Took ${elapsedTime / 1000.0} seconds (avg. ${elapsedTime.toFloat / docsN} ms per doc).")

      val options = TopicModelParams.defaultFor(MalletLDA)
      options.numTopics = 30
      val annotatedCorpus = MalletLDA.annotate(preprocessed, options)
//      val options = TopicModelParams.defaultFor(HDP)
//      val annotatedCorpus = HDP.annotate(preprocessed, options)
      annotatedCorpus
    })

    val outDir = new File("/Users/chrisjr/Desktop/success")
    JsonUtils.toPaperMachines(annotated, outDir)
  }
}
