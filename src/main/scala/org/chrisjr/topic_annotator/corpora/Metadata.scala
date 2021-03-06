package org.chrisjr.topic_annotator.corpora

import play.api.libs.json._
import com.github.tototoshi.csv._
import scala.collection._
import scala.io.Source
import java.io.File
import java.nio.file.{Files, Paths}
import java.net.URI
import scala.util.Try
import org.chrisjr.topic_annotator.utils.JsonUtils

object Metadata {
  type Metadata = JsObject
  def apply(xs: Seq[(String, JsValue)]) = JsObject(xs)
  def basicData(file: File) = {
    Json.obj("itemID" -> JsString(file.toURI.toString))
  }
}

object MetadataCollection {
  import Metadata._
  type MetadataCollection = Map[URI, Metadata]

  implicit class DirWithFind(dir: File) {
    def findFirst(filter: java.io.FilenameFilter): Option[File] = {
      dir.listFiles(filter).headOption
    }
  }

  trait MetadataHandler {
    val fnameFilter: java.io.FilenameFilter
    def parse(file: File): MetadataCollection
  }

  def mkFilter(f: String => Boolean) = new java.io.FilenameFilter { def accept(dir: File, name: String) = f(name) }

  val handlers = Seq(
    new MetadataHandler {
      val fnameFilter = mkFilter(_.toLowerCase.endsWith("metadata.csv"))
      def parse(file: File) = {
        val parentPath = file.getParentFile.toPath
        val parentUri = parentPath.toUri
        val reader = CSVReader.open(file)
        empty ++ ((for {
          row <- reader.allWithHeaders
          filepath <- row.get("filename")
          filename = Paths.get(filepath).getFileName()
          uri = parentUri.relativize(parentPath.resolve(filename).toUri)
          metadata = row.mapValues(JsString)
        } yield uri -> Metadata(metadata.toSeq)).toMap[URI, Metadata])
      }
    },
    new MetadataHandler {
      import org.chrisjr.topic_annotator.utils.JsonUtils._

      val fnameFilter = mkFilter(_.toLowerCase.endsWith("metadata.json"))

      def parse(file: File) = {
        val source = Source.fromFile(file)
        val json = Json.parse(source.map(_.toByte).toArray)
        source.close()
        json.as[Map[URI, Metadata]]
      }
    })

  def emptyFields = mutable.HashMap[String, JsValue with Serializable]()
  def noMetadata = Metadata(emptyFields.toSeq)
  def empty = mutable.HashMap[URI, Metadata]()

  def fromDir(dir: File): MetadataCollection = (for {
    h <- handlers
    fileOpt = dir.findFirst(h.fnameFilter)
    file <- fileOpt
  } yield h.parse(file)).headOption.getOrElse(empty)
}
