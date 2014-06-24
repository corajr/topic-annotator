package org.chrisjr.topics

import org.chrisjr.corpora.Corpus
import org.chrisjr.corpora.CorpusConversions._

object MalletLDA extends TopicModel {
  val stateReader = MalletStateReader
  def trainFrom(corpus: Corpus, options: TopicModelParams) = {
    if (!options.corpusFile.exists) toMalletInstances(corpus, options.corpusFile)
    val args = collection.mutable.ArrayBuffer[String]()
    args ++= Seq("--input", options.corpusFile.getCanonicalPath)
    args ++= Seq("--num-topics", options.numTopics.toString)
    args ++= Seq("--output-state", options.stateFile.getCanonicalPath)  
    cc.mallet.topics.tui.TopicTrainer.main(args.toArray)
  }
}