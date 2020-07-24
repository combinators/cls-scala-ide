
package org.combinators.cls.ide.filter

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

/**
  * This class filters by pattern using power set
  */
class FilterList {
  val emptyGrammar: TreeGrammar = Map.empty

  var tys: Set[String] = Set.empty
  var newRhs = Set.empty[(String, Seq[Type])]

  def newNameArg(forbidIn: String, tm: String) = s"p${
    if (tm == "") {
      ""
    } else {
      "," + tm
    }
  }! $forbidIn"

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
              recSet = recSet ++ Set(a)
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

  def computeRule(rhsNew: (String, Seq[Type]), partPattern: Seq[Muster], partSubPattern: Seq[Muster]): Set[(String, Seq[Type])] = {
    var computeNewRhs = Set.empty[(String, Seq[Type])]
   // var tm = ""
    //var newPArg = ""
    def computeTM(index:Int): String ={
      if (partSubPattern.isEmpty){
        partPattern(index).toString
      }
      else {
        partSubPattern(index).toString +","+ partPattern(index).toString
      }
    }

    rhsNew._2.foldLeft(Seq.empty[Type]) {
      case (leftArgs, rightArgs) =>
        if (leftArgs.nonEmpty) {
          val rhsSide = (rhsNew._1 -> (Seq(Constructor(newNameArg(leftArgs.head.toString(), ""))) ++ (Constructor(newNameArg(rightArgs.toString(), computeTM(leftArgs.size))) +: Seq())))
          computeNewRhs = computeNewRhs + rhsSide
        } else {
          val newRhsArgs: Seq[Type] = if (rhsNew._2.tail.nonEmpty) {
            Seq(Constructor(newNameArg(rhsNew._2.tail.head.toString(), "")))
          } else {
            rhsNew._2.tail
          }
          computeNewRhs = computeNewRhs + (rhsNew._1 -> (leftArgs ++ (Constructor(newNameArg(rightArgs.toString(), computeTM(leftArgs.size))) +: newRhsArgs)))
        }
        leftArgs :+ rightArgs
    }
    (computeNewRhs)
  }

  var matchedStar: Boolean = false
  var matchedPat: Boolean = false
  var matches: Boolean = false

  def isMatched(pattern: Muster, rhs: (String, Seq[Type])): Option[Seq[Muster]]= {
    pattern match {
      case Term(c, pats) if rhs._1 == c && pats.size == rhs._2.size =>
        matches = true
        matchedPat = true
        Some(pats)
      case Star() =>
        matchedStar = true
        None
      case _ =>
        matchedPat = false
        None
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
              matches = false
              matchedStar = false
              for (subPat <- partPattern) {
                 val subPats = isMatched(subPat, rhs)
              if(matchedPat){
                  val pats = isMatched(pattern, rhs)
                if (matchedPat) {
                  pats match {
                    case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, subPats.get)
                    case None =>
                  }
                }else {
                matchedPat =true
                subPats match{
                  case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, Seq.empty)
                  case None =>
                }
              }
              }
              }
              if(!matchedPat) {
                val pts = isMatched(pattern, rhs)
                pts match {
                  case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, Seq.empty)
                  case None =>
                }
              }

          }
          if (!matchedStar && !matches) {
            var newArgs = Seq.empty[Type]
            if (rhs._2.nonEmpty) {
              for (e <- rhs._2) {
                newArgs = newArgs ++ Seq(Constructor(newNameArg(e.toString, "")))
              }

              newRhs = newRhs + (rhs._1 -> newArgs)
            }
            else {
              newRhs = newRhs + rhs
            }
          } else {
            if (matchedStar) {
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
            val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[Type], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                   val (newrhs, bool) = pat match {
                      case Term(pp, ppats) if (grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats.size))) =>
                        //if (pats.tail.head match {
                        //                      case Term(c2, _) =>
                        //                        grammar(rhs._2.tail.head).head._1 == c2
                        //                    })
                        // if (pat match {case Term(pp, ppats) if (grammar(arg).exists(e => (e._1 == pp) && (e._2.size == ppats))) =>

                        val newArg = "<" + "p" + "," + "p" + (args.indexOf(arg) + 1).toString + "!" + arg.toString() + ">"

                        (newRhss + ((combinator -> (leftArgs ++ (Constructor(newArg) +: rightArgs.tail)))), true)

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



