package org.combinators.cls.ide.filter

import org.combinators.cls.ide.translator.{Apply, Combinator, Failed, Rule}
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

/**
  * This class filters by pattern using power set
  */
class FilterApply {
  val emptyGrammar: TreeGrammar = Map.empty
  var tys: Set[String] = Set.empty

  def newNameArg(forbidIn: String, tm: Set[ApplicativePattern]) = s"${tm.toSeq.sortBy(p=>p.toString).mkString("{", "; ", "}")}! $forbidIn"

  def translatePatternToApply(pattern:Muster):ApplicativePattern={
    pattern match {
      case Term(n, terms) => if (terms.isEmpty) {
        CombinatorPattern(n)
      }else{
        val newTerms = terms.dropRight(1)
        ApplyPattern(translatePatternToApply(Term(n, newTerms)), translatePatternToApply(terms.reverse.head))
      }
      case Star() => StarPattern()
    }
  }

  def forbidApply(grammar: Set[Rule], pattern:ApplicativePattern): Set[Rule] ={
    var pPartGrammar: Set[Rule] = Set.empty
    println("patt", pattern)
    val subpatternSet =  pattern match {
      case ApplyPattern(x,y) => mkSetPat(x) ++ mkSetPat(y)
      case CombinatorPattern(n) => mkSetPat(CombinatorPattern(n))
      case _ => Set(StarPattern())
    }
    for (p <- subpatternSet.subsets()) {
      pPartGrammar = pPartGrammar++ forbidInApplyGrammar(grammar, pattern, Set(pattern)++p)
    }
    pPartGrammar
  }
  

  def computeLeftAndRightArgsOfApply(pattern: Set[ApplicativePattern]): ((Set[ApplicativePattern], Set[ApplicativePattern])) = {
    var patternLeft: Set[ApplicativePattern] = pattern
    var patternRight: Set[ApplicativePattern] = pattern
    pattern foreach {
      case ApplyPattern(p1, p2) =>
        patternRight = patternRight + p2
        patternLeft = patternLeft + p1
      case _ => ()
    }
    (patternLeft, patternRight)
  }

  def forbidInApplyGrammar(grammar: Set[Rule], pattern: ApplicativePattern, subsetOfPowerSet: Set[ApplicativePattern]): Set[Rule] = {
    var partGrammar: Set[Rule] = Set.empty
    if(subsetOfPowerSet.contains(StarPattern())) {
      partGrammar
    }else{
      val hasApplyPattern = subsetOfPowerSet.exists(p=>p.isApplyPattern)
      val (partPatternLeft, partPatternRight): (Set[ApplicativePattern],Set[ApplicativePattern])= computeLeftAndRightArgsOfApply(subsetOfPowerSet)
      grammar foreach {
        case Combinator(tgt, n) =>
          if (!subsetOfPowerSet.contains(CombinatorPattern(n))) {
            partGrammar = partGrammar + Combinator(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), n)
          }
        case Apply(tgt, fType, aType) if(hasApplyPattern) =>
          partGrammar = partGrammar +
            Apply(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), Constructor(newNameArg(fType.toString(), partPatternLeft)),
              Constructor(newNameArg(aType.toString(), Set(pattern)))) +
            Apply(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), Constructor(newNameArg(fType.toString(), Set(pattern))),
              Constructor(newNameArg(aType.toString(), partPatternRight)))
        case Apply(tgt, fType, aType) => partGrammar = partGrammar + Apply(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)),
          Constructor(newNameArg(fType.toString(), Set(pattern))),
          Constructor(newNameArg(aType.toString(), Set(pattern))))
      }
      partGrammar
    }
  }

  def mkSetPat(pattern: ApplicativePattern): Set[ApplicativePattern] = {
    var recSet = Set.empty[ApplicativePattern]
    var pats = Set.empty[ApplicativePattern]
    pattern match {
      case CombinatorPattern(n) => recSet = recSet + CombinatorPattern(n)
      case ApplyPattern(x, y) => recSet = recSet +ApplyPattern(x, y) ++mkSetPat(x) ++ mkSetPat(y)
      case StarPattern() => recSet = recSet + StarPattern()

    }
    recSet
  }


  def isMatched(pattern: Set[Muster], rhs: (String, Seq[Type])): (Option[Seq[Muster]], Seq[Muster])= {
    var matches: Seq[Muster] = Seq.empty
    var patArgs: Option[Seq[Muster]] = None
    pattern.foreach(p=> p match {
      case Term(c, pats) if rhs._1 == c && pats.size == rhs._2.size =>
        println("patter", p)
        matches = matches :+ p
        patArgs = Some(pats)
      case Star() =>
        println("patter*", p)
        matches = matches :+ p
        patArgs= None
      case _ =>
        println("patter_", p)
        patArgs = None
    })
    (patArgs, matches)
  }

  def forbidIn(grammar: TreeGrammar, pattern: Muster, n: Type, rhss: Set[(String, Seq[Type])]): (TreeGrammar, Boolean) = {
    val newLhs = "<" + "p" + "!" + n.toString() + ">"
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[Type])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(c, pats) if c == combinator && (pats.size == args.size) =>
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[Type], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, _), (pat, arg)) =>
                    val (newrhs, bool) = pat match {
                      case Term(pp, ppats) if grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats.size)) =>
                        //if (pats.tail.head match {
                        //                      case Term(c2, _) =>
                        //                        grammar(rhs._2.tail.head).head._1 == c2
                        //                    })
                        // if (pat match {case Term(pp, ppats) if (grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats))) =>

                        val newArg = "<" + "p" + "," + "p" + (args.indexOf(arg) + 1).toString + "!" + arg.toString() + ">"

                        (newRhss + (combinator -> (leftArgs ++ (Constructor(newArg) +: rightArgs.tail))), true)

                      case _ =>
                        (newRhss, false)
                    }
                    if (bool) {
                      ((leftArgs :+ arg, rightArgs.tail),
                        newRhss ++ newrhs,
                        additionalGrammar,
                        true)
                    }
                    else {
                      ((leftArgs :+ arg, rightArgs.tail),
                        newRhss,
                        additionalGrammar,
                        true)
                    }
                }
              if (nextMatched) {
                (nextRhss, nextAdditional, matched || nextMatched)
              } else {
                (newRhss + (combinator -> args), additionalGrammar, matched)
              }
            case Star() => (newRhss, additionalGrammar, true)
            case _ => (newRhss + (combinator -> args), additionalGrammar, matched)
          }
      }
    (additionalGrammar + (Constructor(newLhs) -> newRhss), matched)
  }
}



