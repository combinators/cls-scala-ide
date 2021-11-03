package org.combinators.cls.ide.inhabitation

import org.combinators.cls.git.{EmptyResults, Results}
import org.combinators.cls.ide.DebuggerController
import org.combinators.cls.interpreter.ReflectedRepository
import org.scalatest._
import org.combinators.cls.types._

class BCLTest extends FunSpec {

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

  val taxonomy: Taxonomy =
    Taxonomy
      .empty
      .merge(Taxonomy("Char")
        .addSubtype("Int"))
      .merge(Taxonomy("String"))

  def addAll(k: NonEmptyKinding): NonEmptyKinding =
    k.addOption(Constructor("Char"))
      .addOption(Constructor("Int"))
      .addOption(Constructor("String"))

  val kinding: Kinding =
    addAll(Kinding(Variable("alpha"))).merge(addAll(Kinding(Variable("beta"))))
  val testChannel = new DebugMsgChannel()

  lazy val Gamma: BoundedCombinatoryLogicDebugger =
    new BoundedCombinatoryLogicDebugger(testChannel, kinding, SubtypeEnvironment(taxonomy.underlyingMap), mapTest)

  lazy val refRepo = ReflectedRepository(mapTest, taxonomy, FiniteSubstitutionSpace.empty, Gamma2)
  lazy val Gamma2 = BoundedCombinatoryLogicDebugger.algorithm(testChannel)
  val subtypeEnvironment = SubtypeEnvironment(taxonomy.underlyingMap)
  val tree = Gamma2.apply(kinding,subtypeEnvironment, mapTest)
  val reflectedRepository2 = ReflectedRepository(mapTest, substitutionSpace = FiniteSubstitutionSpace.empty, classLoader = this.getClass.getClassLoader)

  describe(Gamma2.toString) {
    describe("|- ? : String") {
      testChannel.reset()
      val tgt = Constructor("List", Constructor("String"))
      val tgt1 = Seq(tgt)
      val results = Gamma.inhabit(tgt)
      val algResult = tree.apply(tgt1)
      val cannotUseCombinator = CannotUseCombinator("map", Constructor("List", Constructor("Char")), Seq(Constructor("List", Arrow(Constructor("Int"), Constructor("Char")))))
      val bclDebuggerInfo = BclDebugger(Gamma, kinding, subtypeEnvironment, refRepo.combinators, tgt1)
      it(s"should push $cannotUseCombinator") {
        assert(!testChannel.debugOutput.contains(cannotUseCombinator))
      }
      it(s"should push $bclDebuggerInfo") {
        assert(!testChannel.debugOutput.contains(bclDebuggerInfo))
      }
      it("should not be empty") {
        assert(results.nonEmpty)
      }
      it("should be equal") {
        assert(algResult.equals(results))
      }
    }
  }
}