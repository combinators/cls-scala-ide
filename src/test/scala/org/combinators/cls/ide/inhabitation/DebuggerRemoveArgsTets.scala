package org.combinators.cls.ide.inhabitation

import org.combinators.cls.types.{Arrow, Constructor, SubtypeEnvironment, Taxonomy}
import org.scalatest.FunSpec

/**
  * Created by Anna on 23.08.2017.
  */

class DebuggerRemoveWithArgsTest extends FunSpec {


  val naturalNumbers =
    Map(
      "One" -> Arrow(Constructor("A"), Constructor("C")),
      "Two" -> Arrow(Constructor("B"), Constructor("C"))


    )
  val taxonomy =
    Taxonomy("A")
      .addSubtype("B")
      .merge(Taxonomy("B")
        .addSubtype("A"))

  lazy val testChannel = new TestChannel()
  val Gamma = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(taxonomy.underlyingMap), naturalNumbers)

  describe(Gamma.toString) {
    describe("|-? : C") {
      val tgt = Constructor("C")
      val result = Gamma.inhabit(tgt)
      testChannel.reset()
      val cannotInhabitBecauseOfSubtype = Set(CannotInhabitBacauseOfSubtype("One", Seq(Constructor("A"))), CannotUseCombinator("One", Constructor("C"), Seq(Constructor("A"))), CannotInhabitType(Constructor("B")), CannotUseCombinator("Two", Constructor("C"), Seq(Constructor("B"))), CannotInhabitType(Constructor("C")))
      val pruneTest = Set(CannotInhabitType(Constructor("B")), CannotUseCombinator("Two", Constructor("C"), Seq(Constructor("B"))), CannotInhabitType(Constructor("C")))
      val cannotInhabitSubtype = CannotInhabitBacauseOfSubtype("One", Seq(Constructor("A")))

      /*
      it(s"should push $cannotInhabitSubtype") {
        assert(testChannel.debugOutput.contains(cannotInhabitSubtype))
      }*/
      it(s"should not push $pruneTest") {
        assert(!testChannel.debugOutput.sameElements(pruneTest))
      }


    }
  }

}
