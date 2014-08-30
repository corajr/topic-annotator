package org.chrisjr.topic_annotator.corpora

import org.chrisjr.topic_annotator.utils.Math._

import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConversions._
import scala.collection.GenIterable

class CorpusScorer(corpus: Corpus, minDf: Int = 3) {
  def sumByKey[A](tuples: Iterable[(A, Int)]) = {
    val m = scala.collection.mutable.Map.empty[A, Int].withDefaultValue(0)
    for ((k, v) <- tuples) m += (k -> (v + m(k)))
    m
  }
  def maxByKey[A](tuples: Iterable[(A, Int)]) = {
    val m = scala.collection.mutable.Map.empty[A, Int].withDefaultValue(0)
    for ((k, v) <- tuples) m += (k -> math.max(v, m(k)))
    m
  }

  //  def sumByKey[A](tuples: GenIterable[(A, Int)]) = {
  //    tuples.groupBy(_._1).mapValues(_.unzip._2.sum)
  //  }
  //
  //  def maxByKey[A](tuples: GenIterable[(A, Int)]) = {
  //    tuples.groupBy(_._1).mapValues(_.unzip._2.max)
  //  }

  lazy val vocab = {
    val vocabSet = new collection.mutable.LinkedHashSet[String]
    for (
      doc <- corpus.documents.seq;
      tokenStr <- doc.tokens.map(_.string)
    ) {
      vocabSet.add(tokenStr)
    }
    //    println(s"${vocabSet.size} distinct words")
    vocabSet.iterator.zipWithIndex.toMap
  }

  lazy val vocabArray = vocab.toSeq.sortBy(_._2).unzip._1

  lazy val tfs = {
    val alwaysOne = Stream.continually(1)
    (for {
      doc <- corpus.documents.seq
      tokens = doc.tokens.map { x => vocab(x.string) }.seq
    } yield sumByKey(tokens zip alwaysOne))
  }

  lazy val tfMaxima = maxByKey(tfs.flatten)
  lazy val tfOverall = sumByKey(tfs.flatten)
  lazy val df = sumByKey(tfs.flatMap { _ map { x => (x._1, 1) } })

  lazy val dfScore = vocab.mapValues(df)

  lazy val tfidf = {
    val D = corpus.documents.size
    (for {
      (word, i) <- vocab;
      tf = tfMaxima(i);
      d = df(i);
      idf = if (d >= minDf) log2(D / d) else 0
    } yield (word, tf * idf))
  }

  lazy val logent = {
    val D = corpus.documents.size
    val logD = log2(D + 1)
    val tflogs = (0 until tfMaxima.size).map { i => log2(tfMaxima(i) + 1) }
    val ent = Array.fill[Double](df.size)(0.0)
    for (
      docCounts <- tfs;
      (idx, count) <- docCounts;
      p_i_j = count.toDouble / tfOverall(idx)
    ) {
      if (df(idx) >= minDf) ent(idx) += p_i_j * log2(p_i_j)
      else ent(idx) = -logD
    }
    ent transform { x => 1 + (x / logD) }
    vocabArray zip ((tflogs, ent).zipped map { _ * _ })
  }
}

object CorpusScorer {
  sealed trait ScorerType
  case object MinDf extends ScorerType
  case object TfIdf extends ScorerType
  case object LogEnt extends ScorerType
}