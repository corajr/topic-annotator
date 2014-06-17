package org.chrisjr.topics

import org.chrisjr.corpora._

class StateAnnotator(val state: GibbsState) extends CorpusTransformer {
  val assignments = state.assignments.map(_.topic).iterator

  def process(doc: Document) = doc.copy(tokens = doc.tokens.map { token => token.copy(topic = assignments.next)})
}