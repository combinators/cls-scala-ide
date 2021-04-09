package org.combinators.cls.ide.filter

import org.combinators.cls.ide.translator.{Apply, Combinator, Failed, Rule}
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}
import scala.collection.parallel.ParSet

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
  def reachableRules(grammar: Set[Rule], tgt: Type, checkedTypes: Set[Type]): (Set[Type],Set[Rule]) ={
    var reachableGrammar: Set[Rule] = Set.empty
    var newCheckedTypes = checkedTypes + tgt
    grammar.foreach(r => if (r.target.equals(tgt)){r match {
      case Combinator(tar, n) =>
        newCheckedTypes=newCheckedTypes+tar
          reachableGrammar = reachableGrammar + Combinator(tar, n)
          Combinator(tar, n)

      case Apply(tar, fType, aType) =>
        if(!newCheckedTypes.contains(fType)) {
          newCheckedTypes = newCheckedTypes + tar
          val reachableRecFtype = reachableRules(grammar, fType, newCheckedTypes)
          newCheckedTypes = newCheckedTypes ++ reachableRecFtype._1
          reachableGrammar = reachableGrammar++ reachableRecFtype._2
        }
          if(!newCheckedTypes.contains(aType)){
            val reachableRecAtype = reachableRules(grammar, aType, newCheckedTypes)
            newCheckedTypes = newCheckedTypes ++ reachableRecAtype._1
            reachableGrammar = reachableGrammar++ reachableRecAtype._2
          }
        reachableGrammar = reachableGrammar+Apply(tgt, fType, aType)
        Apply(tgt, fType, aType)

      case _ => ()
    }})
    (newCheckedTypes,reachableGrammar)
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
      grammar foreach {
        case Combinator(tgt, n) =>
          if (!subsetOfPowerSet.contains(CombinatorPattern(n))) {
            partGrammar = partGrammar + Combinator(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), n)
          }
        case Apply(tgt, fType, aType) if(hasApplyPattern) =>
          val (partPatternLeft, partPatternRight): (Set[ApplicativePattern],Set[ApplicativePattern])= computeLeftAndRightArgsOfApply(subsetOfPowerSet)
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
  def prune(rules: Set[Rule]): Set[Rule] = {
    val parRules = rules.par
    lazy val groundTypes = groundTypesOf(parRules)
    def keepGround: PartialFunction[Rule, Rule] = {
      case Apply(tgt, _, _) if !groundTypes.contains(tgt) => Failed(tgt)
      case app @ Apply(_, arr, tgt)
        if groundTypes.contains(arr) && groundTypes.contains(tgt) =>
        app
      case c @ Combinator(_, _) => c
      case f @ Failed(_)        => f
    }
    parRules.collect(keepGround).seq.toSet
  }
  final def groundTypesOf(rules: ParSet[Rule]): ParSet[Type] = {
    def groundStep(previousGroundTypes: ParSet[Type]): ParSet[Type] = {
      rules.par.aggregate(previousGroundTypes)(
        {
          case (s, Apply(sigma, arr, tgt))
            if s.contains(arr) && s.contains(tgt) =>
            s + sigma
          case (s, _) => s
        },
        { case (s1, s2) => s1.union(s2) }
      )
    }
    var lastGround: ParSet[Type] = ParSet.empty[Type]
    var nextGround: ParSet[Type] = rules.collect {
      case Combinator(target, _) => target
    }
    while (lastGround.size < nextGround.size) {
      lastGround = nextGround
      nextGround = groundStep(lastGround)
    }
    nextGround
  }
  def isInfinite(rules: Set[Rule], target: Type): (Boolean, Set[Rule]) = {
    val grRule = groupedRules(rules)
    println("gggg", grRule)
    def visit(seen: Set[Type], start: Type, vRules:Set[Rule] ): (Boolean, Set[Rule] )  = {
      if (seen.contains(start)) {
        val newvRules = vRules++rules.filterNot(_.target==start)
        println("true", newvRules)
        (true, newvRules)
      }
      else {
        var newvRules = vRules
        (grRule(start).exists {
          case Apply(_, lhs, rhs) =>
            //println("1111", start)
            //println("vvvv", seen)
            val v1 = visit(seen + start, lhs, vRules)
            //println("v111", v1)
            newvRules = newvRules ++v1._2
            val v2 =  visit(seen + start, rhs, newvRules)
            newvRules = newvRules ++v2._2
            //println("v222", v2)
            v1._1 || v2._1
          case _ => false
        }, newvRules)
      }
    }
    val (isVisited, visitedRules) = visit(Set.empty, target, Set.empty)
    (!isEmpty(rules, target) && isVisited, visitedRules)
  }

  def isEmpty(rules:Set[Rule], target: Type): Boolean =
    rules.exists(_ == Failed(target)) || rules.forall(_.target != target)

  def groupedRules(rules:Set[Rule]): Map[Type, Set[Rule]] = {
      rules.groupBy(_.target)
    }

}



