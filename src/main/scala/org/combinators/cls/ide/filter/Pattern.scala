package org.combinators.cls.ide.filter

sealed trait Muster
  case class Star() extends Muster
  case class Term(name: String, args: Seq[Muster]) extends Muster


