package org.chrisjr.utils

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import sun.misc.BASE64Encoder
import java.net.URI

import org.chrisjr.corpora._

object JsonUtils {
  import MetadataCollection._

  val uriReads: Reads[URI] = __.read[String].map(URI.create _)
  val uriWrites: Writes[URI] = (__.write[String]).contramap({ x: URI => x.toString })
  implicit val uriFormat: Format[URI] = Format(uriReads, uriWrites)

  implicit object metadataFormat extends Format[Metadata] {
    def writes(metadata: Metadata) = {
      JsObject(metadata.fields.toSeq)
    }
    def reads(json: JsValue): JsResult[Metadata] = {
      val fields = json.as[JsObject].fields
      val serializable = fields.filter(_._2.isInstanceOf[JsValue with Serializable])
        .map(x => (x._1, x._2.asInstanceOf[JsValue with Serializable]))
      JsSuccess(Metadata(emptyFields ++ serializable.toMap[String, JsValue with Serializable]))
    }
  }

  implicit object metadataCollectionFormat extends Format[MetadataCollection] {
    def writes(mc: MetadataCollection) = {
      JsObject(mc.map(x => x._1.toString -> JsObject(x._2.fields.toSeq)).toSeq)
    }
    def reads(json: JsValue): JsResult[MetadataCollection] = {
      JsSuccess(json.as[JsObject].fields.map(x => URI.create(x._1) -> x._2.as[Metadata]).toMap[URI, Metadata])
    }
  }

  lazy val b64enc = new sun.misc.BASE64Encoder()
  lazy val b64dec = new sun.misc.BASE64Decoder()
  def topicsToBase64(topics: Array[Float]): String = {
    val bb = java.nio.ByteBuffer.allocate(topics.length * 4)
    bb.asFloatBuffer().put(topics)
    b64enc.encode(bb)
  }

  def base64ToTopics(s: String): Array[Float] = {
    val bb = b64dec.decodeBufferToByteBuffer(s)
    val topics = new Array[Float](bb.capacity() / 4)
    bb.asFloatBuffer().get(topics)
    topics
  }
}