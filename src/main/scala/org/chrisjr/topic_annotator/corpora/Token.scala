package org.chrisjr.topic_annotator.corpora

case class Token(start: Int, end: Int, string: String, topic: Int = -1)
