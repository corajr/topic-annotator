package org.chrisjr.topic_annotator.topics

import org.chrisjr.topic_annotator.corpora.Corpus
import org.chrisjr.topic_annotator.corpora.CorpusConversions._
import org.chrisjr.topic_annotator.corpora.Document

abstract class MalletModel extends TopicModel {
  val stateReader = MalletStateReader

  lazy val takeOverLogging = {
    import org.slf4j.bridge.SLF4JBridgeHandler
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install();
    ()
  }
}

object MalletLDA extends MalletModel {
  def trainFrom(corpus: Corpus, options: TopicModelParams) = {
    if (!options.corpusFile.exists) toMalletInstances(corpus, options.corpusFile)
    val args = collection.mutable.ArrayBuffer[String]()
    args ++= Seq("--input", options.corpusFile.getCanonicalPath)
    args ++= Seq("--num-topics", options.numTopics.toString)
    args ++= Seq("--optimize-interval", 20.toString)
    args ++= Seq("--diagnostics-file", new java.io.File(options.outputDir, "diag.xml").getCanonicalPath)
    args ++= Seq("--output-state", options.stateFile.getCanonicalPath)

    takeOverLogging
    cc.mallet.topics.tui.TopicTrainer.main(args.toArray)
  }
}

object MalletDMR extends MalletModel {
  def trainFrom(corpus: Corpus, options: TopicModelParams) = {
    if (!options.corpusFile.exists) toDmrInstances(corpus, options.corpusFile)

    takeOverLogging
    val training = cc.mallet.types.InstanceList.load(options.corpusFile)
    val numTopics = options.numTopics
    val lda = new cc.mallet.topics.DMRTopicModel(numTopics)
	lda.setOptimizeInterval(100)
	lda.setTopicDisplay(100, 10)
	lda.addInstances(training)
	lda.estimate()

	lda.writeParameters(options.dmrParamFile)
	lda.printState(options.stateFile) 
  }
}

