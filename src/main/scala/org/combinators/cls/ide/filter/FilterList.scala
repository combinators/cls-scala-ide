
package org.combinators.cls.ide.filter

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

class FilterList {
  val emptyGrammar: TreeGrammar = Map.empty

  var tys: Set[String] = Set.empty
  var newRhs = Set.empty[(String, Seq[Type])]

  def newNameArg(forbidIn: String, tm: String) = s"p${if (tm == "") {""} else {"," + tm}}! $forbidIn"
  var newArg = ""

  def forbid(grammar: TreeGrammar, pattern: Muster): TreeGrammar = {
    var pPartGrammar = emptyGrammar
    var recSet = Set.empty[Muster]
    var pats = Set.empty[Muster]

    def mkSetPat(pattern: Muster): Set[Muster] = {
      pattern match {
        case Term(name, args) =>
          if (args.nonEmpty) {
            for (a <- args) {
              pats = mkSetPat(a)
              recSet = recSet | pats
            }
            Set(Term(name, pats.toList))
          }
          else {
            Set(Term(name, Seq.empty))
          }
        case Star() =>
          Set(Star())
      }
    }

    mkSetPat(pattern)
    for (p <- recSet.subsets()) {
      val merge = pPartGrammar.toSeq ++ forbidPP(grammar, pattern, p, (recSet.toSeq.indexOf(p) + 1).toString).toSeq
      val grouped = merge.groupBy(_._1)
      pPartGrammar = grouped.mapValues(_.flatMap(_._2).toSet)
    }
    pPartGrammar
  }

  def computeRule(rhsNew: (String, Seq[Type]), partPattern: Seq[Muster]): Set[(String, Seq[Type])] = {
    var computeNewRhs = Set.empty[(String, Seq[Type])]
    rhsNew._2.foldLeft(Seq.empty[Type]) {
      case (leftArgs, rightArgs) =>
        newArg = newNameArg(rightArgs.toString(), (partPattern(rhsNew._2.indexOf(rightArgs))).toString)
        if (leftArgs.nonEmpty) {
          val newPArg = newNameArg(leftArgs.head.toString(), "")
          computeNewRhs = computeNewRhs + (rhsNew._1 -> (Seq(Constructor(newPArg)) ++ (Constructor(newArg) +: Seq())))
        } else {
          val newRhsArgs: Seq[Type] = if (rhsNew._2.tail.nonEmpty) {
            Seq(Constructor(newNameArg(rhsNew._2.tail.head.toString(), "")))
          } else {
            rhsNew._2.tail
          }
          computeNewRhs = computeNewRhs + (rhsNew._1 -> (leftArgs ++ (Constructor(newArg) +: newRhsArgs)))
        }
        leftArgs :+ rightArgs
    }
    (computeNewRhs)
  }

  var matchedStar: Boolean = false
  var matchedPat: Boolean = false

  def isMatched(grammar: TreeGrammar, pattern: Muster, rhs: (String, Seq[Type])) = {
    pattern match {
      case Term(c, pats) if rhs._1 == c && pats.size == rhs._2.size =>
        matchedPat = true
        newRhs = newRhs ++ computeRule(rhs, pats)
      case Star() =>
        matchedStar = true
      case _ =>
    }
  }

  def forbidPP(grammar: TreeGrammar, pattern: Muster, partPattern: Set[Muster], tm: String): TreeGrammar = {
    var additionalGrammar = emptyGrammar
    grammar.foldLeft(emptyGrammar) {
      case (_, (lhs, rhss)) =>
        var ruleNew = Map.empty[Type, Set[(String, Seq[Type])]]
        newRhs = Set.empty
        for (rhs <- rhss) {
          rhs match {
            case (combinator, args) =>
              matchedPat = false
              matchedStar = false
              for (subPat <- partPattern) {
                isMatched(grammar, subPat, rhs)
              }
              if (!matchedPat){
              isMatched(grammar, pattern, rhs)
          }
          }
          if(!matchedStar && !matchedPat) {
            var newArgs = Seq.empty[Type]
            if(rhs._2.nonEmpty) {
              for (e <- rhs._2) {
                newArgs = newArgs ++ Seq(Constructor(newNameArg(e.toString, "")))
              }
              newRhs = newRhs + (rhs._1->newArgs)
            }
            else {
              newRhs = newRhs + rhs
            }
          }else{
            if (matchedStar){
              newRhs = Set.empty
            }
            newRhs
          }
        }
        ruleNew = ruleNew + (Constructor(s"p${
          if (partPattern.nonEmpty) {
            "," + partPattern.mkString(",")
          } else {
            ""
          }
        }! $lhs") -> newRhs)
        additionalGrammar = additionalGrammar ++ ruleNew
        additionalGrammar
    }
    additionalGrammar
  }


  def forbidIn(grammar: TreeGrammar, pattern: Muster, n: Type, rhss: Set[(String, Seq[Type])]): (TreeGrammar, Boolean) = {
    var newLhs = "<" + "p" + "!" + n.toString() + ">"
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[Type])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(c, pats) if c == combinator && (pats.size == args.size) =>
              // if (pats.size > 0){
              // foreach
              // }
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[Type], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                    println("ooooooo", arg, pat)
                    println("ooooooo", grammar(arg))
                    val (newrhs, bool) = pat match {
                      case Term(pp, ppats) if (grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats.size))) =>
                        //if (pats.tail.head match {
                        //                      case Term(c2, _) =>
                        //                        grammar(rhs._2.tail.head).head._1 == c2
                        //                    })
                        // if (pat match {case Term(pp, ppats) if (grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats))) =>

                        println("ttttt", matched)
                        println("PPPPPPPP", newRhss) // (combinator -> (leftArgs ++ (Constructor("newArg") +: rightArgs.tail)))                        )
                        val newArg = "<" + "p" + "," + "p" + (args.indexOf(arg) + 1).toString + "!" + arg.toString() + ">"

                        (newRhss + ((combinator -> (leftArgs ++ (Constructor(newArg) +: rightArgs.tail)))), true)

                      case _ =>
                        println("ffffff", pat, arg)
                        (newRhss, false)
                    }
                    if (bool) {
                      println("halllllllooooo else")
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



