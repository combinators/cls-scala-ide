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
  def filterCombinators(combinators: Seq[Int], exContext: InterpreterContext): Option[Tree] = {
    val toTree = ModelToTree(exContext)
    val term = ModelToTerm(exContext)
    val assertions: Seq[Term] = Seq.empty
    var tree: Option[Tree] = None
    if(combinators.size>1){
      assertions :+ (for (a <- combinators) {
        //term.unusedCombinator(combinatorInt = a)
        println("aaaaa", a)
        val newTerm = term.unusedCombinator(combinatorInt = a).term
        println("aaaaa", newTerm)
        newTerm

      })
      println("xxxx", assertions)
    //And(assertions)
      if (exContext.interpreter.eval(CheckSat()).toString() contains ("unsat")) {
        val getUnsatCore = exContext.interpreter.eval(GetUnsatCore())
        println("Core + 1:" + getUnsatCore)
        None
      } else {
        exContext.interpreter.eval(GetModel())
        println(s"Tree3: ${toTree.getTree(1)}")
        tree = Some(toTree.getTree(1))
      }
    }
    else{
      val assertion = term.unusedCombinator(combinatorInt = combinators.head)
      exContext.interpreter.eval(assertion)
    }
    tree
  }
}

object Assertions {
  def apply(): Assertions =
    new Assertions {}
}
