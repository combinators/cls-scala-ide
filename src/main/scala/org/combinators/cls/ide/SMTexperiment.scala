
package org.combinators.cls.smt.examples.sort

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.smt.{GrammarFuncImpl, GrammarToModel}
import smtlib.theories.Core.{And, BoolSort, Equals, ITE, Not, True}
import smtlib.theories.Ints.IntSort
import smtlib.trees.Commands.{Assert, DefineFun, FunDef}
import smtlib.trees.Terms.{Forall, Identifier, QualifiedIdentifier, SSymbol, SortedVar}

trait SortExperimentSmtImpl extends GrammarFuncImpl  {
  def idConstrSymbol: SSymbol = SSymbol("idConstr")
  def idConstrC = QualifiedIdentifier(Identifier(idConstrSymbol))
  def iSymbol: SSymbol = SSymbol("i")

  lazy val idConstrFun = DefineFun(FunDef(idConstrSymbol, Seq.empty, BoolSort(),
    Forall(
      SortedVar(positionSymbol, IntSort()), Seq.empty,
      And(
        Not(Equals(inhabitantTerm(lsucc(position)), combinatorNameToTerm("id"))),
        Not(Equals(inhabitantTerm(lsucc(position)), combinatorNameToTerm("inv")))
      ))))

  /*
    ITE(Equals(inhabitantTerm(rsucc(position)), combinatorNameToTerm("id")),
      Equals(inhabitantTerm(lsucc(position)), combinatorNameToTerm("sort")), True()),
  */

  lazy val assertIdConstr = Assert(idConstrC)
  lazy val customCommand = Seq(idConstrFun, assertIdConstr)
}

object SortExperimentSmtImpl{
  def apply(g : TreeGrammar, useBv: Boolean = false, _maxTreeSize: Int = 32) = new SortExperimentSmtImpl{
    val grammar = g
    val useBitvectors = useBv
    val maxTreeSize = _maxTreeSize
  }
}