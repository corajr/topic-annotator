package org.chrisjr.topic_annotator.topics

import org.chrisjr.topic_annotator.corpora.Corpus
import org.chrisjr.topic_annotator.corpora.CorpusConversions._

object MalletLDA extends TopicModel {
  val stateReader = MalletStateReader

  lazy val takeOverLogging = {
    import org.slf4j.bridge.SLF4JBridgeHandler
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install();
    ()
  }

  def trainFrom(corpus: Corpus, options: TopicModelParams) = {
    if (!options.corpusFile.exists) toMalletInstances(corpus, options.corpusFile)
    val args = collection.mutable.ArrayBuffer[String]()
    args ++= Seq("--input", options.corpusFile.getCanonicalPath)
    args ++= Seq("--num-topics", options.numTopics.toString)
    args ++= Seq("--optimize-interval", 20.toString)
    args ++= Seq("--output-state", options.stateFile.getCanonicalPath)

    takeOverLogging
    cc.mallet.topics.tui.TopicTrainer.main(args.toArray)
  }
}
