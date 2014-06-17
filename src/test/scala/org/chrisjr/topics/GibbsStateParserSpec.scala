package org.chrisjr.topics

import org.scalatest._
import org.scalatest.Matchers._
import java.io.File
import GibbsStateDir._

trait SparseMatrixTest {
  def validSparseMatrix[V: Numeric](matrix: => SparseMatrix[V]) = {
    val n = matrix.normalized
    n.values.forall { m => math.abs(m.values.sum - 1.0) < 1e-3 } shouldBe true
  }
}

trait StateParser extends SparseMatrixTest { this: FunSpec =>
  def validGibbsState(state: => GibbsState) = {
    it("should have a non-empty docs-topics matrix") {
      state.docTopics.size should be > 0
      it should behave like validSparseMatrix(state.docTopics)
    }
    it("should have a non-empty topics-terms matrix") {
      state.topicTypes.size should be > 0
      it should behave like validSparseMatrix(state.topicTypes)
    }
  }
}

object GibbsStateDir {
  val sampleStates = new File(getClass.getResource("sample-states").toURI())
  val malletStateFile = new File(sampleStates, "mallet.gz")
  val hdpStateFile = new File(sampleStates, "hdp.txt")
}

trait GibbsStateFixture extends SuiteMixin { this: Suite =>
  val malletState = MalletStateReader.fromFile(malletStateFile)
  val hdpState = HDPStateReader.fromFile(hdpStateFile)

  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
  }
}

class GibbsStateParserSpec extends FunSpec with StateParser with GibbsStateFixture {
  describe("A GibbsStateParser") {
    describe("using a MALLET state") {
      it should behave like validGibbsState(malletState)
    }
    describe("using an HDP state") {
      it should behave like validGibbsState(hdpState)
    }
  }
}