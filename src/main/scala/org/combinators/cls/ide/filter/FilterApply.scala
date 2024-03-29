package org.combinators.cls.ide.filter

import org.combinators.cls.ide.translator.{Apply, Combinator, Failed, Rule}
import org.combinators.cls.inhabitation.{TreeGrammar, prettyPrintTreeGrammar}
import org.combinators.cls.types.{Constructor, Type}

import scala.collection.parallel.ParSet

/**
  * This class filters by pattern using power set
  */
class FilterApply {
  val emptyGrammar: TreeGrammar = Map.empty
  var tys: Set[String] = Set.empty

  def newNameArg(forbidIn: String, tm: Set[ApplicativePattern]) = {
    val newName = tm.toSeq.sortBy(p=>p.toString.length).reverse.mkString("{", "; ", "}")
    s"${newName}! $forbidIn"
  }

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
  def reachableRules(grammar: Set[Rule], tgt: Type, checkedTypes: Set[Rule]): (Boolean,Set[Rule]) ={
    var reachableGrammar: Set[Rule] = checkedTypes
    val parRules = grammar.par
    lazy val groundTypes = groundTypesOf(parRules)
    var completed: Boolean = false
    grammar.foreach(r => if (r.target.equals(tgt) && groundTypes.contains(r.target)) {
      if(reachableGrammar.contains(r)) {
        completed = true
      }
      else{
      r match {
        case Failed(_) => completed
        case Combinator(tar, n) =>
          //newCheckedTypes = newCheckedTypes + tar
          reachableGrammar = reachableGrammar + Combinator(tar, n)
          completed = true
        case Apply(tar, fType, aType) =>
          if (grammar.exists(_.target.equals(fType)) && grammar.exists(_.target.equals(aType))) {
            if (tgt.equals(aType)|| tgt.equals(fType)) reachableGrammar = reachableGrammar + Apply(tar, fType, aType)
            reachableGrammar = reachableGrammar + Apply(tar, fType, aType)
            val reach1 = reachableRules(grammar, fType, reachableGrammar)
              val reach2 = reachableRules(grammar, aType, reach1._2++reachableGrammar)
            if(reach1._1 && reach2._1){
              reachableGrammar = reachableGrammar ++reach1._2++
                reach2._2
              completed = true
            }else{
              completed
            }
      }else{
            completed = false
          }
      }}
    })
    (completed,reachableGrammar)
  }
  def updateGrammar(reachableGrammar:TreeGrammar, ty:Type, rhs: (String, Seq[Type])):TreeGrammar = {
    var newGram = reachableGrammar
    newGram.get(ty) match {
      case Some(e) => newGram = newGram.updated(ty, (e+rhs))
      case None    => newGram += ((ty, Set(rhs)))
    }
    newGram
  }

  def reachableTreeGrammar(grammar: TreeGrammar, tgt: Seq[Type], checkedGrammar: TreeGrammar, toCheck:Set[Type]): (TreeGrammar, Set[Type]) ={
    var reachableGrammar: TreeGrammar = Map.empty
    var currentTypes: TreeGrammar = checkedGrammar
    var typesToCheck: Set[Type] = toCheck
    val probe = grammar.map(_._1)
    if(tgt.forall(t => probe.toSeq.contains(t))){
    val existRule = grammar.filter(k=> tgt.contains(k._1))
    for (elem <- existRule) {
        if(!currentTypes.map(_._1).toSeq.contains(elem._1) && !typesToCheck.contains(elem._1) ){
          for (e <- elem._2){
            if(e._2.isEmpty){
              //reachableGrammar = updateGrammar(reachableGrammar, elem._1, e)
              reachableGrammar = reachableGrammar + elem
              currentTypes = currentTypes + elem
              typesToCheck = typesToCheck + elem._1
            }else{
              typesToCheck = typesToCheck + elem._1
              val rechGr = reachableTreeGrammar(grammar, e._2, currentTypes, typesToCheck)
                reachableGrammar = updateGrammar(reachableGrammar, elem._1, e) ++ rechGr._1
              typesToCheck = typesToCheck ++ rechGr._2
                currentTypes = currentTypes + elem
            }
          }
          }
        //println(prettyPrintTreeGrammar(reachableGrammar))
    }}
    (reachableGrammar, typesToCheck)
    /*val newReach = reachableGrammar.map(e=> e._2.map(k => if (k._2.isEmpty) {
      //checkedTypes= checkedTypes++
    }else{
      (reachableTreeGrammar(grammar,k._2, checkedTypes))
    }))
    println("WWW ", newReach)
    tgt.foreach(e=> if(!currentTypes.contains(e)) {
      currentTypes = currentTypes++tgt
      reachableGrammar.foreach(r => r._2.map(e => if (e._2.nonEmpty) reachableGrammar = reachableGrammar ++ reachableTreeGrammar(grammar, e._2, currentTypes)._1))
    })

   // if(checked && exist)
    //reachableTreeGrammar(grammar, e._2, Map.empty)) )
      /*r match {
      case (t, args) => args.map(e => if(e._2.isEmpty) {
        println("ccc", (t, e))
      }else{
         e._2.foreach(k=> println("rec", reachableTreeGrammar(grammar, k, reachableGrammar)._2))
      })
      case _ => println("rule", r)
    })*/
    (reachableGrammar, true)*/
  }

  def computeLeftAndRightArgsOfApply(pattern: Set[ApplicativePattern]): ((Set[ApplicativePattern], Set[ApplicativePattern])) = {
    var patternLeft: Set[ApplicativePattern] = Set.empty
    var patternRight: Set[ApplicativePattern] = Set.empty
    pattern foreach {
      case ApplyPattern(p1, p2) =>
        patternRight = patternRight++pattern + p2
        patternLeft = patternLeft ++pattern + p1
      case _ => ()
    }
    (patternLeft, patternRight)
  }

 /* def forbidInApplyGrammar(grammar: Set[Rule], pattern: ApplicativePattern, subsetOfPowerSet: Set[ApplicativePattern]): Set[Rule] = {
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
  }*/
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
          val newFtype =Set(pattern) ++ subsetOfPowerSet ++ partPatternRight
          val newAtype = Set(pattern) ++ subsetOfPowerSet ++partPatternLeft
          partGrammar = partGrammar +
            Apply(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), Constructor(newNameArg(fType.toString(), newAtype)),
              Constructor(newNameArg(aType.toString(), Set(pattern)++subsetOfPowerSet))) +
            Apply(Constructor(newNameArg(tgt.toString(), subsetOfPowerSet)), Constructor(newNameArg(fType.toString(), Set(pattern)++subsetOfPowerSet)),
              Constructor(newNameArg(aType.toString(), newFtype)))
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
    def visit(seen: Set[Type], start: Type, vRules:Set[Rule] ): (Boolean, Set[Rule] )  = {
      if (seen.contains(start)) {
        val newvRules = vRules++rules.filterNot(_.target==start)
        (true, newvRules)
      }
      else {
        var newvRules = vRules
        (grRule(start).exists {
          case Apply(_, lhs, rhs) =>
            val v1 = visit(seen + start, lhs, vRules)
            newvRules = newvRules ++v1._2
            val v2 =  visit(seen + start, rhs, newvRules)
            newvRules = newvRules ++v2._2
            v1._1 || v2._1
          case _ => false
        }, newvRules)
      }
    }
    val (isVisited, visitedRules) = visit(Set.empty, target, Set.empty)
    (!isEmpty(rules, target) && isVisited, visitedRules)
  }

  private def isEmpty(rules:Set[Rule], target: Type): Boolean =
    rules.exists(_ == Failed(target)) || rules.forall(_.target != target)

  def groupedRules(rules:Set[Rule]): Map[Type, Set[Rule]] = rules.groupBy(_.target)

}



