package org.chrisjr.topic_annotator.utils

import org.slf4j.{ Logger, LoggerFactory }
import ch.qos.logback.core._
import ch.qos.logback.classic.{ LoggerContext, PatternLayout }
import ch.qos.logback.classic.spi.ILoggingEvent
import java.io._
import ch.qos.logback.classic.Level

object Logging {
  def logTo(file: java.io.File, pattern: String = "%d %5p %t [%c:%L] %m%n)") = {
    val lc = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]

    val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
    val fileAppenderOpt = Option(rootLogger.getAppender("file"))
    val fileAppender = fileAppenderOpt.getOrElse({
      val a = new FileAppender[ILoggingEvent]()
      a.setName("file")
      a
    }).asInstanceOf[FileAppender[ILoggingEvent]]
    fileAppender.stop()
    fileAppender.setFile(file.getCanonicalPath)
    val pl = new PatternLayout()
    pl.setPattern(pattern)
    pl.setContext(lc)
    pl.start()
    fileAppender.setLayout(pl)
    fileAppender.setContext(lc)
    fileAppender.start()

    if (fileAppenderOpt.isEmpty) rootLogger.addAppender(fileAppender)
  }
}