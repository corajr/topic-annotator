package org.chrisjr.topic_annotator.topics

import org.chrisjr.topic_annotator.corpora._
import CorpusConversions._
import java.io._
import de.uni_leipzig.informatik.asv.hdp._
import de.uni_leipzig.informatik.asv.utils._
import uk.org.lidalia.sysoutslf4j.context.SysOutOverSLF4J

object HDPConversions {
  implicit def toCLDAFile(corpus: Corpus) = {
    val ldacPrefix = File.createTempFile("ldac", null)
    val outFile = toLDAC(corpus, ldacPrefix)
    outFile
  }
  implicit def toInputStream(file: File) = new FileInputStream(file)
}

object HDP extends TopicModel {
  lazy val takeOverLogging = {
    SysOutOverSLF4J.sendSystemOutAndErrToSLF4J()
    ()
  }

  val stateReader = HDPStateReader
  import HDPConversions._
  def trainFrom(
    corpus: Corpus,
    options: TopicModelParams) = {
    val corpusFile = toCLDAFile(corpus)
    val topicTermFile = File.createTempFile("topic-term", null)

    takeOverLogging
    HDPGibbsSampler.main(Array(corpusFile.getPath, topicTermFile.getPath, options.stateFile.getPath))
  }
}
