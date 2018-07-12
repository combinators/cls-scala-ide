package org.combinators.cls.ide.inhabitation

import org.combinators.cls.inhabitation._
import org.combinators.cls.types.{FiniteSubstitutionSpace, SubtypeEnvironment}

class BoundedCombinatoryLogicDebugger(debuggerChannel: DebugMessage => Unit, substitutionSpace: FiniteSubstitutionSpace, subtypes: SubtypeEnvironment, Gamma: Repository)
  extends BoundedCombinatoryLogic(substitutionSpace, subtypes, Gamma) {

  override lazy val algorithm: FiniteCombinatoryLogicDebugger =
    new FiniteCombinatoryLogicDebugger(debuggerChannel, subtypes, repository)

}

object BoundedCombinatoryLogicDebugger {
  def algorithm(debuggerChannel: DebugMessage => Unit): InhabitationAlgorithm = {
    case (substitutionSpace, subtypes, repository) =>
      targets => new BoundedCombinatoryLogicDebugger(debuggerChannel, substitutionSpace, subtypes, repository).inhabit(targets: _*)
  }
}