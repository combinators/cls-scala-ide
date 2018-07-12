
package org.combinators.cls.ide.inhabitation

import org.combinators.cls.types.Type


sealed trait DebugMessage

case class CannotUseCombinator(combinatorName: String, tgt: Type, uninhabitedAgrs: Seq[Type]) extends DebugMessage

case class CannotInhabitType(ty: Type) extends DebugMessage

case class CannotInhabitBacauseOfSubtype(combinatorName: String, uninhabitedArgs: Seq[Type]) extends DebugMessage


