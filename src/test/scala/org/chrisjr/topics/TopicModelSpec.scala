package org.chrisjr.topic_annotator.topics

import org.scalatest._
import org.scalatest.Matchers._
import org.chrisjr.topic_annotator.corpora.CorpusFixture
import java.io._
import org.chrisjr.topic_annotator.corpora.Corpus

trait TopicTrainer { this: FunSpec =>
  def trainUsing(model: TopicModel, corpus: => Corpus, stateFile: File = File.createTempFile("assignments", null)) = {
    it("should be able to train a model from a corpus") {
      val options = TopicModelParams.defaultFor(model)
      options.stateFile = stateFile
      model.trainFrom(corpus, options)
    }
  }
}

class TopicModelSpec extends FunSpec with CorpusFixture with TopicTrainer {
  describe("The HDP object") {
//      val stateFile = new File("/Users/chrisjr/Development/workspace/topic-annotator/src/test/resources/org/chrisjr/topics/sample-states/hdp.txt")
    it should behave like trainUsing(HDP, corpus)
  }
  describe("The MalletLDA object") {
//      val stateFile = new File("/Users/chrisjr/Development/workspace/topic-annotator/src/test/resources/org/chrisjr/topics/sample-states/mallet.gz")
    it should behave like trainUsing(MalletLDA, corpus)
  }
}
