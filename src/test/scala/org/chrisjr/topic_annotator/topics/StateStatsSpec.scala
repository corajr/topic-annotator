package org.chrisjr.topic_annotator.topics

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class StateStatsSpec extends FunSpec with StateParser with GibbsStateFixture {
  describe("IMIs") {
    they("should be calculable from a state") {
      val allImis = StateStats.getAllImis(malletState)
      println(allImis)
    }
  }
}
