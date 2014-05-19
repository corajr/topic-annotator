package org.chrisjr.corpora

case class Token(start: Int, end: Int, string: String, topic: Int = -1)