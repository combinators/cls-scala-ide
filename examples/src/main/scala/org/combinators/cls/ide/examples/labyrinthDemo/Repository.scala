package org.combinators.cls.ide.examples.labyrinthDemo

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._

class Repository {
  def intToType(x: Int): Type = {
    Constructor(x.toString)
  }
  def position(pos: (Int, Int)): Type = {
    'Pos (intToType(pos._1), intToType(pos._2))
  }

  @combinator object start {
    def apply: Type = position(0,0)
    val semanticType: Type = position(0,0)

  }

  @combinator object up{
    def apply(pos: Type): Type = position(3,0)
    val semanticType: Type = (position(2,1) =>: position(2,0)) :&: (position(0,1) =>: position(0,0))
  }
  @combinator object down {
    def apply(pos: Type): Type = position(3,0)
    val semanticType: Type = (position(2,0) =>: position(2,1)) :&: (position(0,0) =>: position(0,1))
  }
  @combinator object left {
    def apply(pos: Type): Type = position(3,0)
    val semanticType: Type = position(3,0) =>: position(2, 0)
  }
  @combinator object right {
    def apply(pos: Type):Type = position(2,0)
    val semanticType: Type = position(2,0) =>: position(3, 0)
  }

}