package org.chrisjr.topic_annotator

import java.io.File
import org.chrisjr.topic_annotator.corpora._
import org.chrisjr.topic_annotator.utils._
import org.chrisjr.topic_annotator.topics._
import scala.util.{ Try, Success, Failure }

/**
 * @author ${user.name}
 */
object App {
  val topicsN = -1
  val ldacPrefix: Option[File] = None //Some(new File("/Users/chrisjr/Desktop/ss/ss"))
  val metadataFilename: Option[String] = Some("/Users/chrisjr/Desktop/ss/metadata.tsv")

  def main(args: Array[String]) {
    val inputDirOpt = if (args.length > 0) Some(new File(args(0))) else None
    val corpusFile = if (args.length > 1) Some(new File(args(1))) else None
    val preprocessedCorpusFile = if (args.length > 2) Some(new File(args(2))) else None

    importCorpusAndProcess(inputDirOpt, corpusFile, preprocessedCorpusFile)
    //    compareStates
    //    getMetadata("/Users/chrisjr/Desktop/corpus4595756955954337501dat", "/Users/chrisjr/Desktop/success_scripts/files.tsv")
  }

  def getMetadata(corpusFilename: String, outFilename: String): Unit = saveMetadata(
    Util.unpickle[Corpus](new File(corpusFilename)),
    outFilename)

  def saveMetadata(corpus: Corpus, outFilename: String): Unit = {
    val sep = "\t"
    val metadata = for {
      (doc, i) <- corpus.documents.zipWithIndex;
      md = doc.metadata;
      //      _ = println(md.toString)
      yearOpt = (md.\("issued").\("date-parts"))(0)(0).asOpt[String]
      year <- yearOpt
      labelOpt = (md.\("label")).asOpt[String]
      label <- labelOpt
      journal = (md.\("container-title")).asOpt[String].getOrElse("")
    } yield Seq(i, year.toInt, journal, label)

    val writer = new java.io.PrintWriter(outFilename, "UTF-8")

    val header = Seq("doc", "year", "journal", "label")
    writer.println(header.mkString(sep))
    metadata.foreach { x =>
      writer.println(x.mkString(sep))
    }
    writer.close()
  }

  def compareStates = {
    val dir = new File("/Users/chrisjr/Desktop")
    val dirs = (1 to 5).map(i => new File(dir, s"m$i.$topicsN"))

    def getTW(stateDir: File) = {
      val state = MalletStateReader.fromFile(new File(stateDir, "state"))
      val wordsN = state.wordsN
      state.topicTypes.normalized.mapValues { x =>
        (0 until wordsN).map(x.getOrElse(_, 0.0))
      }
    }

    val tws = dirs.map(getTW)

    var jsds = collection.immutable.HashMap[(Int, Int), Iterable[(Int, Int, Double)]]()

    for (
      i <- 0 until tws.length;
      j <- 0 until i
    ) {
      val jsdsIJ = for {
        (iTopic, topic1) <- tws(i)
        (jTopic, topic2) <- tws(j)
      } yield (iTopic, jTopic, StateStats.jsdMetric(topic1, topic2))

      jsds = jsds.updated((i + 1, j + 1), jsdsIJ)
    }

    Util.pickle(new File(dir, "jsds"), jsds)
  }

  def urlGroup(x: Document): String = {
    val url = (x.metadata \ "URL").asOpt[String]
    if (url.nonEmpty) new java.net.URL(url.get).getHost() else ""
  }

  def filterByLabel(label: String) = {
    def hasLabel(doc: Document) = {
      val labelOpt = (doc.metadata \ "label").asOpt[String]
      labelOpt.nonEmpty && labelOpt.get == label
    }
    new DocumentFilterBy(hasLabel)
  }

  def mkTransformers: Seq[CorpusTransformer] = {
    val stopwords = StopwordRemover.forLang("en").get ++
      StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/stopwords.txt")).get ++
      StopwordRemover.fromFile(new File("/Users/chrisjr/Desktop/success_scripts/author_names.txt")).get

    //    val stem = Snowball.getStemFuncFor("en")
    //    val stemmedStops = new StopwordRemover(stopwords.stopwords.map(stem))

    Seq(new Dedupe,
      new CommonSubstringRemover(minLength = 20, grouping = urlGroup),
      LowercaseTransformer,
      new RegexTransformer("\\W+".r, ""),
      //      filterByLabel("International journals"), //"Successful School articles"),
      // DehyphenationTransformer,
      new MinLengthRemover(4),
      stopwords,
//      new SnowballTransformer("english"),
//      stemmedStops,
//      new ScoreTransformer(topWords = Int.MaxValue, minDf = 3, scorerType = CorpusScorer.MinDf))
      new ScoreTransformer(topWords = 5000, minDf = 3, scorerType = CorpusScorer.LogEnt))
  }

  def importCorpusAndProcess(
    inputDirOpt: Option[File],
    corpusFile: Option[File],
    preprocessedCorpusFile: Option[File]) = {

    val inputDir = inputDirOpt.getOrElse(new File(
      Class.forName("org.chrisjr.topic_annotator.corpora.Corpus")
        .getResource("sample-texts").toURI))
    def annotatedFile(suffix: String = "") = new File(preprocessedCorpusFile.get.getPath + suffix)

    def doOrUnpickle[T <: Serializable](fileOpt: Option[File], block: => T): T = {
      if (fileOpt.nonEmpty && fileOpt.get.exists) Util.unpickle[T](fileOpt.get)
      else {
        val item = block
        if (fileOpt.nonEmpty) Util.pickle(fileOpt.get, item)
        item
      }
    }

    var startTime = System.currentTimeMillis()
    var docsN = 0

    //    for ((i, numTopics) <- Stream.continually(1) zip (Seq(30, 40, 50, 75, 100))) {
    //    for ((i, numTopics) <- (1 to 5) zip (Stream.continually(topicsN))) {
    for ((i, numTopics) <- Seq((1, topicsN))) {
      val outputDir = new File(s"/Users/chrisjr/Desktop/m$i.$numTopics")

      val annotated = doOrUnpickle(Some(annotatedFile(s"$i.$numTopics")), {
        Logging.logTo(new File(outputDir, "log.txt"))

        val preprocessed = doOrUnpickle(preprocessedCorpusFile, {
          val raw = doOrUnpickle(corpusFile, {
            val corpusTry = Corpus.fromDir(inputDir)
            val myCorpus = corpusTry.get
            myCorpus
          })
          docsN = raw.documents.size
          println(s"$docsN documents loaded")

          val transformers = mkTransformers
          raw.transform(transformers)
        })
//        CorpusConversions.toVocab(preprocessed, "/Users/chrisjr/Desktop/sswords.tsv")

        val elapsedTime = System.currentTimeMillis() - startTime
        println(s"Took ${elapsedTime / 1000.0} seconds (avg. ${elapsedTime.toFloat / docsN} ms per doc).")

        if (ldacPrefix.nonEmpty) CorpusConversions.toLDAC(preprocessed, ldacPrefix.get)
        if (metadataFilename.nonEmpty) saveMetadata(preprocessed, metadataFilename.get)

        val modelType: TopicModel = HDP

        val options = TopicModelParams.defaultFor(modelType)
        options.outputDir = outputDir
        options.outputDir.mkdirs()
        options.stateFile = new File(options.outputDir, "state")

        val annotatedCorpus = modelType match {
          case MalletLDA =>
            options.numTopics = numTopics
            MalletLDA.annotate(preprocessed, options)
          case MalletDMR =>
            options.dmrParamFile = new File(options.outputDir, "dmr.parameters")
            options.numTopics = numTopics
            MalletDMR.annotate((new DmrFeatures(Set("time", "journal")))(preprocessed), options)
          case HDP =>
            HDP.annotate(preprocessed, options)
        }
        annotatedCorpus
      })

      val outDir = new File("/Users/chrisjr/Desktop/success")
      JsonUtils.toPaperMachines(annotated, outDir)
      CorpusConversions.toVocab(annotated, new File(outputDir, "words.tsv").getCanonicalPath())
    }
  }
}
