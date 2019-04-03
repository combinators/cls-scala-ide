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

import org.combinators.cls.inhabitation.Repository
import org.combinators.cls.types.{FiniteSubstitutionSpace, SubtypeEnvironment, Type}


sealed trait DebugMessage

case class CannotUseCombinator(combinatorName: String, tgt: Type, uninhabitedAgrs: Seq[Type]) extends DebugMessage

case class CannotInhabitType(ty: Type) extends DebugMessage

//case class CannotInhabitBecauseOfSubtype(combinatorName: String, uninhabitedArgs: Seq[Type]) extends DebugMessage

case class SubtypeOf(tgt: Type, syTy: Type) extends DebugMessage

case class BclDebugger(bclDebugger: BoundedCombinatoryLogicDebugger,
                       substitutionSpace: FiniteSubstitutionSpace,
                       subtypes: SubtypeEnvironment,
                       Gamma: Repository,
                       targets: Seq[Type]) extends DebugMessage


