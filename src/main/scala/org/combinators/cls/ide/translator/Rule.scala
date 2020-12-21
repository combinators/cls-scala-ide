package org.combinators.cls.ide.translator

import org.combinators.cls.types.Type

sealed trait Rule  {
  val target: Type
}
final case class Failed(target: Type) extends Rule

/** Represents types inhabited by a combinator without arguments. */
final case class Combinator(target: Type, combinator: String) extends Rule

/** Represents the application of a term of type `functionType` to a term of type `argumentType` to obtain
  * type `target`. */
final case class Apply(target: Type, functionType: Type, argumentType: Type)
  extends Rule
