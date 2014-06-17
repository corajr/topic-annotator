package org.chrisjr.utils

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import java.net.URI

import org.chrisjr.corpora._

object JsonUtils {
  import MetadataCollection._

  val uriReads: Reads[URI] = __.read[String].map(URI.create _)
  val uriWrites: Writes[URI] = (__.write[String]).contramap({ x: URI => x.toString })
  implicit val uriFormat: Format[URI] = Format(uriReads, uriWrites)

  implicit object metadataFormat extends Format[Metadata] {
    def writes(metadata: Metadata) = {
      JsObject(metadata.toSeq)
    }
    def reads(json: JsValue): JsResult[Metadata] = {
      JsSuccess(json.as[JsObject].fields.toMap[String, JsValue].asInstanceOf[Metadata])
    }
  }

  implicit object metadataCollectionFormat extends Format[MetadataCollection] {
    def writes(mc: MetadataCollection) = {
      JsObject(mc.map( x => x._1.toString -> JsObject(x._2.toSeq)).toSeq)
    }
    def reads(json: JsValue): JsResult[MetadataCollection] = {
      JsSuccess(json.as[JsObject].fields.map(x => URI.create(x._1) -> x._2.as[Metadata]).toMap[URI, Metadata])
    }
  }
}