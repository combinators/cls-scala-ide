package org.combinators.cls.ide.translator

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Arrow, Type}

class TreeGrammarToApplicativeTreeGrammar {

  type MultiArrow = (Seq[Type], Type)

  def translateTGtoATG(grammar: TreeGrammar): Set[Rule] = {
    var rules: Set[Rule] = Set.empty
    var combinatorsSet:Set[String] = Set.empty
    var starTypes:Set[Type] = Set.empty
    val existStar = grammar.exists(_._2.exists(_._1.equals("*")))
    grammar.foreach {
      case (ty, args) => args.foreach {
        case (c, a) if existStar => if (c.equals("*")) {
          starTypes = starTypes + ty
        } else {
          if (a.isEmpty) {
            combinatorsSet = combinatorsSet + c
          }
          starTypes.map(e=> combinatorsSet.map(k=> Combinator(e, k)))
          rules = rules ++ computeFunctionTypes(a, ty, c) ++
            starTypes.flatMap(e => combinatorsSet.map(k => Combinator(e, k))) ++
            starTypes.map(e=> Apply(e, e, e))
        }
        case (c, a) =>
          rules = rules ++ computeFunctionTypes(a, ty, c)
      }
    }
    rules
  }

  def computeFunctionTypes(args: Seq[Type], ty: Type, combinator: String): Set[Rule] = {
    var rules: Set[Rule] = Set.empty
    if (args.nonEmpty) {
      val newFT = Arrow(args.reverse.head, ty)
      rules = rules + Apply(ty, newFT, args.reverse.head) ++
        computeFunctionTypes(args.dropRight(1), newFT, combinator)
    } else {
      rules = rules + Combinator(ty, combinator)
    }
    rules
  }
}


