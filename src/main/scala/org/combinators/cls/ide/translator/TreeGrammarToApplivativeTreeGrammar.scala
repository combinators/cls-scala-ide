package org.combinators.cls.ide.translator

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Arrow, Type}

class TreeGrammarToApplivativeTreeGrammar {

  var rules: Set[Rule] = Set.empty

  def translateTGtoATG(grammar: TreeGrammar): Set[Rule] = {
    rules = Set.empty
    grammar.map {
      case (ty, args) => args.map(k =>
        rules = rules ++ computeFunctionTypes(k._2, ty, k._1)
      )
    }
    rules
  }

  def computeFunctionTypes(args: Seq[Type], ty: Type, combinator: String): Set[Rule] = {
    var argList = args
    var newFT = ty
    if (argList.size >= 1) {
      newFT = Arrow(argList.reverse.head, ty)
      rules = rules + Apply(ty, newFT, argList.reverse.head)
      argList = argList.dropRight(1)
      computeFunctionTypes(argList, newFT, combinator)
    } else {
      rules = rules + Combinator(ty, combinator)
    }
    rules
  }
}
