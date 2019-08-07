package org.combinators.cls.ide

import org.combinators.cls.inhabitation.Tree
import org.combinators.cls.smt._
import org.combinators.cls.types.Type
import smtlib.Interpreter
import smtlib.theories.Core.{And, Equals, ITE}
import smtlib.trees.Commands.{Assert, CheckSat, GetModel, GetUnsatCore}
import smtlib.trees.Terms._


trait Assertions {
  //lazy val model: GrammarToModel =

  //Filter by combinator name
  def filterCombinators(combinators: Int, exContext: InterpreterContext): Option[Tree] = {
    var tree: Option[Tree] = None
    val toTree = ModelToTree(exContext)
    val term = ModelToTerm(exContext)

    val assertion = term.unusedCombinator(combinatorInt = combinators)
    exContext.interpreter.eval(assertion)
    if (exContext.interpreter.eval(CheckSat()).toString() contains ("unsat")) {
    }
    else if (exContext.interpreter.eval(CheckSat()).toString() contains ("unknown")) {
    }
    else{
      //exContext.interpreter.eval(GetModel())
      tree = Some(toTree.getTree(1))
    }
    tree
  }
}
object Assertions {
  def apply(): Assertions = new Assertions {}
}
