package org.chrisjr.corpora

import play.api.libs.json._
import com.github.tototoshi.csv._
import scala.collection._
import scala.io.Source
import java.io.File
import java.net.URI
import scala.util.Try

object MetadataCollection {
  type Metadata = Map[String, JsValue]
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
      val fnameFilter = mkFilter(_.toLowerCase == "metadata.csv")
      def parse(file: File) = {
        val reader = CSVReader.open(file)
        (for {
          row <- reader.allWithHeaders
          uri = new File(file.getParentFile, row.getOrElse("filename", "")).toURI
          metadata = row.mapValues(JsString)
        } yield uri -> metadata).toMap[URI, Metadata]
      }
    },
    new MetadataHandler {
      import org.chrisjr.utils.JsonUtils._

      val fnameFilter = mkFilter(_.toLowerCase == "metadata.json")

      def parse(file: File) = {
        val source = Source.fromFile(file)
        val json = Json.parse(source.map(_.toByte).toArray)
        source.close()
        empty ++ json.as[Map[URI, Metadata]]
      }
    })

  def noMetadata = immutable.Map[String, JsValue]()
  def empty = immutable.Map[URI, Metadata]().withDefaultValue(noMetadata)

  def fromDir(dir: File): MetadataCollection = (for {
    h <- handlers
    fileOpt = dir.findFirst(h.fnameFilter)
    file <- fileOpt
  } yield h.parse(file)).headOption.getOrElse(empty)
}