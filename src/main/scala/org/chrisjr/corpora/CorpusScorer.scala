package org.chrisjr.corpora

import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.JavaConversions._
import scala.collection.GenIterable

class CorpusScorer(corpus: Corpus, minDf: Int = 3) {
  def log2(x: Double) = math.log(x) / math.log(2)

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
    val vocabSet = new ConcurrentSkipListSet[String]
    for (
      doc <- corpus.documents;
      tokenStr <- doc.tokens.map(_.string)
    ) {
      vocabSet.add(tokenStr)
    }
//    println(s"${vocabSet.size} distinct words")
    vocabSet.iterator.zipWithIndex.toMap
  }

  lazy val tfs = {
    val alwaysOne = Stream.continually(1)
    (for {
      doc <- corpus.documents.seq
      tokens = doc.tokens.map { x => vocab(x.string) }.seq
    } yield sumByKey(tokens zip alwaysOne))
  }

  lazy val tfMaxima = maxByKey(tfs.flatten)
  lazy val df = sumByKey(tfs.flatMap { _ map { x => (x._1, 1) } })

  lazy val tfidf = {
    val D = corpus.documents.size
    (for {
      (word, i) <- vocab;
      tf = tfMaxima(i);
      d = df(i);
      idf = if (d >= minDf) log2(D / d) else 0
    } yield (word, tf * idf))
  }
}