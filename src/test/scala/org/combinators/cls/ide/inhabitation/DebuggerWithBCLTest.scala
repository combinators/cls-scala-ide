
package org.combinators.cls.ide.inhabitation

import org.combinators.cls.types._
import org.scalatest.FunSpec

/**
  * Created by Anna on 25.08.2017.
  */
class DebuggerWithBCLTest extends FunSpec {
  val mapTest =
    Map(
      "map" ->
        Arrow(
          Arrow(Variable("alpha"), Variable("beta")),
          Arrow(
            Constructor("List", Variable("alpha")),
            Constructor("List", Variable("beta"))
          )
        ),
      "l" -> Constructor("List", Constructor("Int")),
      "f" -> Arrow(Constructor("Char"), Constructor("String"))
    )

  val taxonomy =
    Taxonomy
      .empty
      .merge(Taxonomy("Char")
        .addSubtype("Int"))
      .merge(Taxonomy("String"))

  def addAll(k: NonEmptyKinding): NonEmptyKinding =
    k.addOption(Constructor("Char"))
      .addOption(Constructor("Int"))
      .addOption(Constructor("String"))

  val kinding =
    addAll(Kinding(Variable("alpha"))).merge(addAll(Kinding(Variable("beta"))))

  lazy val testChannel = new TestChannel()

  val Gamma = new BoundedCombinatoryLogicDebugger(testChannel, kinding , SubtypeEnvironment(taxonomy.underlyingMap), mapTest)


  describe(Gamma.toString){
    describe("|- ? : impossible") {
      val tgt = Constructor("List", Constructor("impossible"))
      testChannel.reset()
      val cannotInhabitType = CannotInhabitType(tgt)
      val results = Gamma.inhabit(tgt)

      it("should be empty") {
        assert(results.isEmpty)
      }

      it(s"should push $cannotInhabitType") {
        assert(testChannel.debugOutput.contains(cannotInhabitType))
      }
    }
  }

}

