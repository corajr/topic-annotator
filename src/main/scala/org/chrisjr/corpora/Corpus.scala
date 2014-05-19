/**
 *
 */
package org.chrisjr.corpora

import java.io.File
import scala.collection.GenSeq
import scala.util.{Try, Success, Failure}
import java.io.FileNotFoundException

case class Corpus(documents: GenSeq[Document], transformers: Seq[CorpusTransformer] = Seq()) {
  def transform(newTransformers: Seq[CorpusTransformer]): Corpus = {
//    val aggregate = newTransformers.reduce(CorpusTransformer.combine)
//    aggregate.apply(this)
    newTransformers.foldLeft(this) { (x, y) => y(x)}
  }
}

object Corpus {
  val textFilenameFilter = new java.io.FilenameFilter { def accept(dir: File, name: String) = name.endsWith(".txt") }
  def fromDir(dir: File): Try[Corpus] = {
    if (!dir.exists() || !dir.isDirectory()) return Failure(new FileNotFoundException(s"${dir.getPath} does not exist or is not a directory."))
    for {
      fileList <- Try(dir.listFiles(textFilenameFilter))
      documents = fileList.map {y: File => Document.fromTextFile(y) }
    } yield Corpus(documents.collect { case Success(x) => x; case Failure(e) => throw e })
  }
}