package org.combinators.cls.ide

import org.combinators.cls.inhabitation.Tree
import org.combinators.cls.smt._
import org.combinators.cls.types.Type
import smtlib.Interpreter
import smtlib.theories.Core.{Equals, ITE}
import smtlib.trees.Commands.{Assert, CheckSat, GetModel, GetUnsatCore}
import smtlib.trees.Terms._


trait Assertions {
  //lazy val model: GrammarToModel =

  //Filter by combinator name
  def filterCombinators(combinatorName: Int, exContext: InterpreterContext): Option[Tree] = {
    val toTree = ModelToTree(exContext)
    val term = ModelToTerm(exContext)

    val assertion = term.unusedCombinator(combinatorInt = combinatorName)
    println("Comb: ", combinatorName)
    println("Tree: " + assertion.term + " " + toTree.getTree(1))
    var tree: Option[Tree] = None
    println("addAssertions2: " + assertion)
    exContext.interpreter.eval(assertion)
    if (exContext.interpreter.eval(CheckSat()).toString() contains ("unsat")) {
      val getUnsatCore = exContext.interpreter.eval(GetUnsatCore())
      println("Core + 1:" + getUnsatCore)
      None
    } else {
      exContext.interpreter.eval(GetModel())
      println(s"Tree3: ${toTree.getTree(1)}")
      tree = Some(toTree.getTree(1))
    }
    tree
  }

}

object Assertions {
  def apply(): Assertions =
    new Assertions {}
}
