
package org.combinators.cls.ide.filter

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

/**
  * This class filters by pattern using power set
  */
class FilterList {
  val emptyGrammar: TreeGrammar = Map.empty

  var tys: Set[String] = Set.empty

  def newNameArg(forbidIn: String, tm: String) = s"p${
    if (tm == "") {
      ""
    } else {
      "," + tm
    }
  }! $forbidIn"

  def mkSetPat(pattern: Muster): Set[Muster] = {
    var recSet = Set.empty[Muster]
    var pats = Set.empty[Muster]
    recSet = pattern match {
      case Term(name, args) =>
        for (a <- args) {
          pats = mkSetPat(a)
          recSet= recSet++Set(a)++pats

          // Set(Term(name, pats.toList))
        }
        recSet
      case Star() =>
        Set(Star())
    }
    recSet
  }

  def forbid(grammar: TreeGrammar, pattern: Muster): TreeGrammar = {
    var pPartGrammar = emptyGrammar
    val recSet = mkSetPat(pattern)
    val recSetNew: Set[Set[Muster]]= Set() + Set(pattern) ++ recSet.map(e => Set(pattern) + e)
    for (p <- recSet.subsets()) {
      val merge = pPartGrammar.toSeq ++ forbidPP(grammar, p).toSeq

      val grouped = merge.groupBy(_._1)
      pPartGrammar = grouped.mapValues(_.flatMap(_._2).toSet)
    }
    pPartGrammar
  }

  def computeRule(rhsNew: (String, Seq[Type]), partPattern: Seq[Muster], partSubPattern: Option[Seq[Muster]]): Set[(String, Seq[Type])] = {
    var computeNewRhs = Set.empty[(String, Seq[Type])]
    // var tm = ""
    //var newPArg = ""
    def computeTM(index:Int): String ={
      if (partSubPattern.isEmpty){
        partPattern(index).toString
      }
      else {
        partSubPattern.get(index).toString +"," + partPattern(index).toString
      }
    }

    rhsNew._2.foldLeft(Seq.empty[Type]) {
      case (leftArgs, rightArgs) =>
        if (leftArgs.nonEmpty) {
          val rhsSide = rhsNew._1 -> (Seq(Constructor(newNameArg(leftArgs.head.toString(), "")))
            ++ (Constructor(newNameArg(rightArgs.toString(), computeTM(leftArgs.size))) +: Seq()))
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
    computeNewRhs
  }

  //var matches: Boolean = false

  /* def isMatched(pattern: Muster, rhs: (String, Seq[Type])): (Option[Seq[Muster]], Boolean)= {

     var matchedStar: Boolean = false

     pattern match {
       case Term(c, pats) if rhs._1 == c && pats.size == rhs._2.size =>
        // matches = true
        ( Some(pats), matchedStar)
       case Star() =>
         matchedStar = true
         (None, matchedStar)
       case _ =>
         (None, matchedStar)
     }
   }*/


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
  /*
    def forbidPP(grammar: TreeGrammar, pattern: Muster, partPattern: Set[Muster]): TreeGrammar = {
      var additionalGrammar = emptyGrammar
      var newRhs = Set.empty[(String, Seq[Type])]
      grammar.foldLeft(emptyGrammar) {
        case (_, (lhs, rhss)) =>
          var ruleNew = Map.empty[Type, Set[(String, Seq[Type])]]
          newRhs = Set.empty
          for (rhs <- rhss) {
                if (partPattern.isEmpty) {
                  val (matchPattern, isStar) = isMatched(pattern, rhs)
                  if (!matchPattern.isEmpty) {
                    newRhs = newRhs ++ computeRule(rhs, matchPattern.get, None)
                  } else {
                    if(isStar){
                      newRhs = Set.empty
                    }
                }
                  ruleNew = ruleNew + (Constructor(s"p${""}! $lhs") -> newRhs)
                }
            else{
                for (subPat <- partPattern) {
                   val (subPats, _) = isMatched(subPat, rhs)
                if(!subPats.isEmpty){
                  val (pats, _) = isMatched(pattern, rhs)
                  if (!pats.isEmpty) {
                        newRhs = newRhs ++ computeRule(rhs, pats.get, subPats)
                  }else {
                  newRhs = newRhs ++ computeRule(rhs, subPats.get, None)
                }
                }
                }

                  ruleNew = ruleNew + (Constructor(s"p${
                      "," + partPattern.mkString(",")
                  }! $lhs") -> newRhs)
                }
                if(!matchedPat) {
                  val pts = isMatched(pattern, rhs)
                  pts._1 match {
                    case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, None)
                    case None =>
                  }
                }


            if (!matchedStar && !matchedPat) {
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
          additionalGrammar = additionalGrammar ++ ruleNew
          additionalGrammar
      }
      additionalGrammar
    }*/

  def forbidPP(grammar: TreeGrammar, allPattern: Set[Muster]): TreeGrammar = {

    println("<<<", allPattern)
    var additionalGrammar = emptyGrammar
    var newRhs = Set.empty[(String, Seq[Type])]
    grammar.foldLeft(emptyGrammar) {
      case (_, (lhs, rhss)) =>
        var ruleNew = Map.empty[Type, Set[(String, Seq[Type])]]
        newRhs = Set.empty
        for (rhs <- rhss) {

            /*e match{
            case Term(name, args)  =>if (name == rhs._1 && args.size ==rhs._2.size)
              println("true", e)
              e
            case Star() =>
              println("star", e)
              e
              }*/

          val (subPats, matches) = isMatched(allPattern, rhs)
          println("xxx", rhs)
        //  println("<<<<", matches1)
          if(!matches.isEmpty){
            val computeRule1 = computeRule(rhs, matches, None)
            println("rule", computeRule1)
            newRhs = newRhs ++ computeRule1

          }else{
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
          }

          /*
          if (!matchedStar && !matchedPat) {
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
          }*/
        }
        ruleNew = ruleNew + (Constructor(s"p${
          if (allPattern.nonEmpty) {
            "," + allPattern.mkString(",")
          } else {
            ""
          }
        }! $lhs") -> newRhs)
        println("....", additionalGrammar)
        additionalGrammar = additionalGrammar ++ ruleNew
        additionalGrammar
    }

    println("....1111", additionalGrammar)
    additionalGrammar
  }

  /*
  def forbidPP(grammar: TreeGrammar, pattern: Muster, partPattern: Set[Muster]): TreeGrammar = {

      var additionalGrammar = emptyGrammar
      var newRhs = Set.empty[(String, Seq[Type])]
      grammar.foldLeft(emptyGrammar) {
        case (_, (lhs, rhss)) =>
          var ruleNew = Map.empty[Type, Set[(String, Seq[Type])]]
          newRhs = Set.empty
          for (rhs <- rhss) {
            rhs match {
              case (_, _) =>
                matchedPat = false
                //matches = false
                matchedStar = false
                for (subPat <- partPattern) {
                   val subPats = isMatched(subPat, rhs)
                if(matchedPat){
                    val pats = isMatched(pattern, rhs)
                  if (matchedPat) {
                    pats match {
                      case Some(value) => println("pats some", subPats.get)
                    println("pats some1", pats.get)
                        val computeRule1 = computeRule(rhs, value, subPats)
                        println("rule", computeRule1)
                        newRhs = newRhs ++ computeRule1
                      case None => println("pats", pats)
                    }
                  }else {
                  matchedPat =true
                  subPats match{
                    case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, None)
                    case None =>
                  }
                }
                }
                }
                if(!matchedPat) {
                  val pts = isMatched(pattern, rhs)
                  pts match {
                    case Some(value) => newRhs = newRhs ++ computeRule(rhs, value, None)
                    case None =>
                  }
                }

            }
            if (!matchedStar && !matchedPat) {
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

  */

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



