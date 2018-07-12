
package org.combinators.cls.ide.inhabitation

import org.combinators.cls.types._
import org.scalatest._

/**
  * Created by Anna on 18.08.2017.
  */
class DebuggerPruneTest extends FunSpec{

  val garbageCombinators =
    Map(
      "f" ->
        Arrow(
          Constructor("Int"),
          Constructor("Goal")
        ),
      "x" -> Constructor("Int"),
      "garbage1" ->
        Arrow(
          Constructor("Garbage1"),
          Intersection(Constructor("Int"), Constructor("Garbage2"))
        ),
      "garbage2" ->
        Arrow(
          Constructor("Garbage2"),
          Constructor("Garbage1")
        )
    )
  val taxonomy =
    Taxonomy("Int")
      .merge(Taxonomy("Garbage1"))
      .merge(Taxonomy("Garbage2"))
      .merge(Taxonomy("Goal"))

  lazy val testChannel = new TestChannel()

  val Gamma = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(taxonomy.underlyingMap), garbageCombinators)


  describe("|- ? : impossible") {
    val tgt = Constructor("impossible")
    testChannel.reset()
    val results = Gamma.inhabit(tgt)
    println("result", results)
    val cannotUseGarbage2 = CannotUseCombinator("garbage2", Constructor("Garbage1"), Seq(Constructor("Garbage2")))
    val cannotUseGarbage1 = CannotUseCombinator("garbage1", Constructor("Garbage2"), Seq(Constructor("Garbage1")))
    val cannotInhabitGarbage1 = CannotInhabitType(Constructor("Garbage1"))
    val cannotInhabitGarbage2 = CannotInhabitType(Constructor("Garbage2"))
    println("Impossible", testChannel.debugOutput)

    it(s"should push $cannotUseGarbage2") {
      assert(testChannel.debugOutput.contains(cannotUseGarbage2))
    }
    it(s"should push $cannotUseGarbage1") {
      assert(testChannel.debugOutput.contains(cannotUseGarbage1))
    }
    it(s"should push $cannotInhabitGarbage1") {
      assert(testChannel.debugOutput.contains(cannotInhabitGarbage1))
    }
    it(s"should push $cannotInhabitGarbage2") {
      assert(testChannel.debugOutput.contains(cannotInhabitGarbage2))
    }


  }

  describe("|- ? : Goal") {
    val tgt = Constructor("Goal")
    testChannel.reset()
    val results = Gamma.inhabit(tgt)
    val cannotUseGarbage1 = CannotUseCombinator("garbage1", Constructor("Int"), Seq(Constructor("Garbage1")))
    val canUseCombinator = CannotUseCombinator("f", Constructor("Int"), Seq(Constructor("Goal")))
    val canUseCombinator1 = CannotUseCombinator("x", Constructor("Int"), Seq(Constructor("Int")))
    println("GOAL",testChannel.debugOutput)
    it(s"should push $cannotUseGarbage1") {
      assert(testChannel.debugOutput.contains(cannotUseGarbage1))
    }
    it(s"schould not push $canUseCombinator") {
      assert(!testChannel.debugOutput.contains(canUseCombinator))
    }
    it(s"should not push $canUseCombinator1") {
      assert(!testChannel.debugOutput.contains(canUseCombinator1))
    }

  }


}
