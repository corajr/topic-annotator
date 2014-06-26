package org.chrisjr.corpora

import play.api.libs.json._
import com.github.tototoshi.csv._
import scala.collection._
import scala.io.Source
import java.io.File
import java.net.{URI, URL}
import scala.util.Try
import org.chrisjr.utils.JsonUtils

case class Metadata(fields: mutable.HashMap[String, _ <: JsValue with Serializable])

object Metadata {
  import JsonUtils._
  def basicData(file: File): Metadata = {
    val fields = mutable.HashMap("itemID" -> JsString(file.toURL.toString))
    Metadata(fields)
  }
}

object MetadataCollection {
  type MetadataCollection = Map[URL, Metadata]

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
          url = new File(file.getParentFile, row.getOrElse("filename", "")).toURL
          metadata = row.mapValues(JsString)
        } yield url -> Metadata(emptyFields ++ metadata)).toMap[URL, Metadata])
      }
    },
    new MetadataHandler {
      import org.chrisjr.utils.JsonUtils._

      val fnameFilter = mkFilter(_.toLowerCase.endsWith("metadata.json"))

      def parse(file: File) = {
        val source = Source.fromFile(file)
        val json = Json.parse(source.map(_.toByte).toArray)
        source.close()
        empty ++ json.as[Map[URL, Metadata]]
      }
    })

  def emptyFields = mutable.HashMap[String, JsValue with Serializable]()
  def noMetadata = Metadata(emptyFields)
  def empty = mutable.HashMap[URL, Metadata]()

  def fromDir(dir: File): MetadataCollection = (for {
    h <- handlers
    fileOpt = dir.findFirst(h.fnameFilter)
    file <- fileOpt
  } yield h.parse(file)).headOption.getOrElse(empty)
}