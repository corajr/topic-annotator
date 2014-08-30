package org.chrisjr.topic_annotator.corpora

import java.io._
import cc.mallet.pipe._
import cc.mallet.types._

import Util._

object CorpusConversions {

  // each line: [total number of tokens] [type]:[count]
  // counts: .ldac, aux files: .dmap, .vocab
  def toLDAC(corpus: Corpus, ldacPrefix: File) = {
    def mkFile(prefix: File, suffix: String) = new File(prefix.getParentFile(), prefix.getName + suffix)
    val vocabFile = mkFile(ldacPrefix, ".vocab")
    val ldacFile = mkFile(ldacPrefix, ".ldac")
    val dmapFile = mkFile(ldacPrefix, ".dmap")
    val scorer = new CorpusScorer(corpus)
    val vocab = scorer.vocabArray
    val tfs = scorer.tfs

    val ldacWriter = new PrintWriter(ldacFile)
    for (docTf <- tfs) {
      val total = docTf.keys.size
      val counts = docTf.toSeq.sorted.map { case (word, count) => s"${word+1}:$count" } mkString " "
      ldacWriter.println(s"$total $counts")
    }
    ldacWriter.close()

    val vocabWriter = new PrintWriter(vocabFile)
    for (word <- vocab) {
      vocabWriter.println(word)
    }
    vocabWriter.close()

    val dmapWriter = new PrintWriter(dmapFile)
    for (docName <- corpus.documents.map(_.uri)) {
      dmapWriter.println(docName)
    }
    dmapWriter.close()
    ldacFile
  }

  def toMalletInstances(corpus: Corpus, malletFile: File) = {
    val pipe = new TokenSequence2FeatureSequence
    val instances = new InstanceList(pipe)
    for (doc <- corpus.documents) {
      val ts = new TokenSequence
      val tokenIterator = doc.tokens.iterator
      while (tokenIterator.hasNext) {
        ts.add(tokenIterator.next.string)
      }
      val instance = new cc.mallet.types.Instance(ts, null, doc.uri, null);
      instances.addThruPipe(instance)
    }
    pickle(malletFile, instances)
    malletFile
  }

}
