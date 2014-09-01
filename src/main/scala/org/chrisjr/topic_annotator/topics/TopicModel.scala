package org.chrisjr.topic_annotator.topics

import org.chrisjr.topic_annotator.corpora.Corpus
import java.io.File
import java.nio.file.Files
import org.chrisjr.topic_annotator.corpora.Document

class TopicModelParams extends Serializable {
  var outputDir = Files.createTempDirectory("topicmodel").toFile
  var corpusFile = new File(outputDir, "corpus.dat")
  var stateFile = new File(outputDir, "assignments.txt")
  var numTopics = -1 // unused by HDP
  var dmrParamFile = new File(outputDir, "dmr.parameters")
  var orderedWords = true
}

object TopicModelParams {
  def defaultFor[T <: TopicModel](t: T) = {
    val params = new TopicModelParams
    t match {
      case MalletLDA =>
        params.numTopics = 25
      case MalletDMR =>
        params.numTopics = 25
      case HDP =>
        params.orderedWords = false
        ()
    }
    params
  }
}

trait TopicModel {
  val stateReader: org.chrisjr.topic_annotator.topics.GibbsStateReader
  def trainFrom(corpus: Corpus, options: TopicModelParams): Unit
  def annotate(corpus: Corpus, options: TopicModelParams = TopicModelParams.defaultFor(this)): Corpus = {
    if (!options.stateFile.exists) trainFrom(corpus, options)
    val state = stateReader.fromFile(options.stateFile)
    val annotator = new StateAnnotator(state, options)
    annotator(corpus)
  }
}
