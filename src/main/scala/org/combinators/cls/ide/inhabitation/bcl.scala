/*
 * Copyright 2018 Anna Vasileva
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.cls.ide.inhabitation

import org.combinators.cls.inhabitation.{Repository, TreeGrammar, _}
import org.combinators.cls.types.{FiniteSubstitutionSpace, SubtypeEnvironment, Type}

class BoundedCombinatoryLogicDebugger(debuggerChannel: DebugMessage => Unit, substitutionSpace: FiniteSubstitutionSpace, subtypes: SubtypeEnvironment, Gamma: Repository)
  extends BoundedCombinatoryLogic(substitutionSpace, subtypes, Gamma) {
  override lazy val algorithm: FiniteCombinatoryLogicDebugger = {
    new FiniteCombinatoryLogicDebugger(debuggerChannel, subtypes, repository)
  }
}

object BoundedCombinatoryLogicDebugger {
 //def algorithm(debuggerChannel: DebugMessage => Unit): InhabitationAlgorithm = {
 def algorithm(debuggerChannel: TestChannel): InhabitationAlgorithm = {

    case (substitutionSpace, subtypes, repository) =>
      targets =>
        val bclDebugger = new BoundedCombinatoryLogicDebugger(debuggerChannel, substitutionSpace, subtypes, repository)
        debuggerChannel(BclDebugger(bclDebugger, substitutionSpace, subtypes, repository, targets))
        bclDebugger.inhabit(targets: _*)

    }


}