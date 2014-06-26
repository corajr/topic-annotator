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
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory

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
  val logger = Logger(LoggerFactory.getLogger("topic-annotator"))

  val textFilenameFilter = new java.io.FilenameFilter { def accept(dir: File, name: String) = name.toLowerCase.endsWith(".txt") }

  def fromDir(dir: File): Try[Corpus] = {
    if (!dir.exists() || !dir.isDirectory()) return Failure(new FileNotFoundException(s"${dir.getPath} does not exist or is not a directory."))

    val dirUri = dir.toURI
    val metadataColl = MetadataCollection.fromDir(dir)
    for {
      fileList <- Try(dir.listFiles(textFilenameFilter))
      documents = fileList.map { y: File =>
        val uri = dirUri.relativize(y.toURI)
        Document.fromTextFile(y, metadata = metadataColl.getOrElse(uri, Metadata.basicData(y)))
      }
      successes = documents.collect { case Success(x) => x }
      _ = (documents.collect { case Failure(e) => e.getStackTraceString }).foreach(logger.error(_))
    } yield Corpus(successes)
  }
}