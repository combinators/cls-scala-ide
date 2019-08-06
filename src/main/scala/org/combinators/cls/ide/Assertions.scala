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
    println("Term", term.intToCombiantorTerm(combinators))
    println("Tree", toTree)

    val assertion = term.unusedCombinator(combinatorInt = combinators)
    exContext.interpreter.eval(assertion)
    println("hallo assertion", assertion)
    println("hallo filter", exContext.interpreter.eval(assertion))
    println("hallo 2", exContext.interpreter.eval(CheckSat()))
    if (exContext.interpreter.eval(CheckSat()).toString() contains ("unsat")) {
      println("unsat")
    }
    else if (exContext.interpreter.eval(CheckSat()).toString() contains ("unknown")) {
      println("unknown")
    }
    else{
      println("getting model", Some(toTree.getTree(1)))
      //exContext.interpreter.eval(GetModel())
      tree = Some(toTree.getTree(1))
    }
    tree
  }
}
object Assertions {
  def apply(): Assertions = new Assertions {}
}
