package org.chrisjr.topic_annotator.utils

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.Random

class MathSpec extends FunSpec {
  import Math._

  describe("entropy") {
    it("should be uniform for a rolled die") {
      val virtualDiceEntropy = entropy(Seq(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
      virtualDiceEntropy should equal (log2(6.0))

      val rand = new Random(1)
      val diceRolls = (0 to 10000).map { _ => rand.nextInt(6) }.groupBy(identity).mapValues(_.sum.toDouble).values
      val diceEntropy = entropy(diceRolls)
      diceEntropy should be <= (log2(6.0))
    }
  }
}

class CategoricalSpec extends FunSpec {
  val epsilon = 1e-3
  val weights = Seq(("A", 50), ("B", 25), ("C", 15), ("D", 10))
  val sequence = (for ((s, weight) <- weights) yield Seq.fill(weight)(s)).flatten

  describe("A Categorical[String]") {
    it("can be created from a sequence") {
      val cats = Categorical.fromSeq(sequence)
    }

    it("can be created from a map") {
      val cats = Categorical.fromMap(weights.toMap)
    }

    it("should give back values according to weights") {
      val n = 1e6.toInt
      val cats = Categorical.fromSeq(sequence)
      val answers = for (_ <- 0 until n) yield cats.get
      val counts = answers.groupBy(identity).mapValues(_.size)
      for ((s, weight) <- weights) {
        math.abs(counts(s) - weight*(n/100)).toDouble / n should be < epsilon
      }
    }
  }
}