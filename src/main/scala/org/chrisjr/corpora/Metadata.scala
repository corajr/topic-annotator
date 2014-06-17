package org.chrisjr.corpora

import play.api.libs.json._
import com.github.tototoshi.csv._
import scala.collection._
import scala.io.Source
import java.io.File
import java.net.URI
import scala.util.Try

case class Metadata(fields: immutable.HashMap[String, JsValue with Serializable])

object MetadataCollection {
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
        val reader = CSVReader.open(file)
        empty ++ ((for {
          row <- reader.allWithHeaders
          uri = new File(file.getParentFile, row.getOrElse("filename", "")).toURI
          metadata = row.mapValues(JsString)
        } yield uri -> Metadata(emptyFields ++ metadata)).toMap[URI, Metadata])
      }
    },
    new MetadataHandler {
      import org.chrisjr.utils.JsonUtils._

      val fnameFilter = mkFilter(_.toLowerCase.endsWith("metadata.json"))

      def parse(file: File) = {
        val source = Source.fromFile(file)
        val json = Json.parse(source.map(_.toByte).toArray)
        source.close()
        empty ++ json.as[Map[URI, Metadata]]
      }
    })

  def emptyFields = immutable.HashMap[String, JsValue with Serializable]()
  def noMetadata = Metadata(emptyFields)
  def empty = immutable.HashMap[URI, Metadata]().withDefaultValue(noMetadata)

  def fromDir(dir: File): MetadataCollection = (for {
    h <- handlers
    fileOpt = dir.findFirst(h.fnameFilter)
    file <- fileOpt
  } yield h.parse(file)).headOption.getOrElse(empty)
}