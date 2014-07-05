package org.chrisjr.utils

import java.nio.file.{ Paths, Files }
import java.nio.charset.StandardCharsets
import java.io.File
import java.io.PrintWriter
import java.net.URI
import org.chrisjr.corpora._
import org.chrisjr.topics._
import play.api.libs.functional.syntax.toContraFunctorOps
import play.api.libs.json._
import java.nio.ByteOrder

object JsonUtils {
  import MetadataCollection._
  import Metadata._

  val uriReads: Reads[URI] = __.read[String].map(URI.create _)
  val uriWrites: Writes[URI] = (__.write[String]).contramap({ x: URI => x.toString })
  implicit val uriFormat: Format[URI] = Format(uriReads, uriWrites)

  implicit object metadataCollectionFormat extends Format[MetadataCollection] {
    def writes(mc: MetadataCollection) = {
      JsObject(mc.map(x => x._1.toString -> JsObject(x._2.fields.toSeq)).toSeq)
    }
    def reads(json: JsValue): JsResult[MetadataCollection] = {
      JsSuccess(json.as[JsObject].fields.map(x => URI.create(x._1) -> x._2.as[Metadata]).toMap[URI, Metadata])
    }
  }

  def toSerializableJson[T](o: T)(implicit tjs: Writes[T]): JsValue with Serializable = {
    val value = Json.toJson(o)
    if (value.isInstanceOf[JsString]) value.asInstanceOf[JsString]
    else if (value.isInstanceOf[JsNumber]) value.asInstanceOf[JsNumber]
    else if (value.isInstanceOf[JsBoolean]) value.asInstanceOf[JsBoolean]
    else if (value.isInstanceOf[JsObject]) value.asInstanceOf[JsObject]
    else if (value.isInstanceOf[JsArray]) value.asInstanceOf[JsArray]
    else JsNull
  }

  lazy val b64enc = new sun.misc.BASE64Encoder()
  lazy val b64dec = new sun.misc.BASE64Decoder()
  def topicsToBase64(topics: Array[Float]): String = {
    val bb = java.nio.ByteBuffer.allocate(topics.length * 4)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.asFloatBuffer().put(topics)
    b64enc.encode(bb).replaceAll("\n", "")
  }

  def base64ToTopics(s: String): Array[Float] = {
    val bb = b64dec.decodeBufferToByteBuffer(s)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    val topics = new Array[Float](bb.capacity() / 4)
    bb.asFloatBuffer().get(topics)
    topics
  }

  /**
   * @param corpus the corpus with
   * @param dir
   */
  def toPaperMachines(corpus: Corpus, dir: File) = {
    val stateAnnotatorOpt = corpus.transformers.find(_.isInstanceOf[StateAnnotator])
    require(stateAnnotatorOpt.nonEmpty, "Corpus has not been annotated with topics!")

    import StateAnnotator._

    val state = stateAnnotatorOpt.get.asInstanceOf[StateAnnotator].state
    val vocab = corpus.vocab

    val numericVocab = vocab.zipWithIndex.toMap

    val topicLabels = state.topicLabels(vocab, 20)
    val imis = StateStats.getAllImis(state)

    val topicLabelObjs = JsObject(
      (for (topic <- 0 until state.topicsN) yield topic.toString -> JsArray(for {
        word <- topicLabels(topic)
        i = numericVocab(word)
        prob = state.tw(topic)(i)
        imi = imis(topic)(i)
      } yield Json.obj("text" -> word, "prob" -> prob, "imi" -> imi))))

    def dtAsString(doc: Int): Option[String] = {
      state.dt.get(doc).map { topics =>
        topicsToBase64((0 until state.topicsN).map(topics.getOrElse(_, 0.0).toFloat).toArray)
      }
    }

    val metadata = JsObject(corpus.documents.seq.view.zipWithIndex.map {
      case (x, i) =>
        val m = collection.mutable.HashMap[String, JsValue]()
        m ++= x.metadata.fields
        dtAsString(i).foreach { t => m += ("topics" -> JsString(t)) }
        val itemID = m("itemID").as[String]
        itemID -> Metadata(m.toSeq)
    })

    val data = Json.obj("TOPIC_LABELS" -> topicLabelObjs, "DOC_METADATA" -> metadata)

    val dirPath = Paths.get(dir.toURI)
    val dataFile = dirPath.resolve("js").resolve("mallet_data.js")
    dataFile.getParent.toFile.mkdirs()

    Files.write(dataFile, ("var data=" + Json.stringify(data) + ";").getBytes(StandardCharsets.UTF_8))

    val textPath = dirPath.resolve("js").resolve("texts")
    textPath.toFile.mkdirs()
    for (doc <- corpus.documents if doc.tokens.size > 0) {
      val itemID = (doc.metadata \ "itemID").as[String]
      val docFile = textPath.resolve(itemID)
      Files.write(docFile, doc.topicsHTML.getBytes(StandardCharsets.UTF_8))
    }
  }
}