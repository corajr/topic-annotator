/**
 *
 */
package org.chrisjr.corpora

import java.io.File
import scala.collection.GenSeq
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }
import java.io.FileNotFoundException
import java.net.URI

import MetadataCollection._

case class Corpus(documents: GenSeq[Document], transformers: Seq[CorpusTransformer] = Seq()) {
  def transform(newTransformers: Seq[CorpusTransformer]): Corpus = {
    //    val aggregate = newTransformers.reduce(CorpusTransformer.combine)
    //    aggregate.apply(this)
    newTransformers.foldLeft(this) { (x, y) => y(x) }
  }
  def vocab = {
    documents.flatMap(_.tokens.map(_.string)).distinct.seq
  }
}

object Corpus {
  val textFilenameFilter = new java.io.FilenameFilter { def accept(dir: File, name: String) = name.toLowerCase.endsWith(".txt") }

  def fromDir(dir: File): Try[Corpus] = {
    if (!dir.exists() || !dir.isDirectory()) return Failure(new FileNotFoundException(s"${dir.getPath} does not exist or is not a directory."))

    val metadataColl = MetadataCollection.fromDir(dir)
    for {
      fileList <- Try(dir.listFiles(textFilenameFilter))
      documents = fileList.map { y: File =>
        Document.fromTextFile(y, metadata = metadataColl.getOrElse(y.toURL, Metadata.basicData(y)))
      }
      successes = documents.collect { case Success(x) => x }
      _ = (documents.collect { case Failure(e) => e.getStackTraceString }).foreach { println(_) }
    } yield Corpus(successes)
  }
}