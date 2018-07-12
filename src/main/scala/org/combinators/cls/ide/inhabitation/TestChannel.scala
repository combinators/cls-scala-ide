
package org.combinators.cls.ide.inhabitation



class TestChannel extends Function[DebugMessage, Unit] {
  import scala.collection.mutable

  val debugOutput: mutable.Set[DebugMessage] = mutable.Set.empty

  def apply(msg: DebugMessage): Unit = debugOutput.add(msg)

  def reset(): Unit = debugOutput.clear()
}