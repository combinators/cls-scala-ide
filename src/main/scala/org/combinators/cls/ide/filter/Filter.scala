package org.combinators.cls.ide.filter

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

class Filter {
  val emptyGrammar: TreeGrammar = Map.empty
  var tys: Set[String] = Set.empty
  def pushName(name:String, index:Int = 3): String = {
    name + StringUtils.repeat("'", index)
  }
  def forbid(grammar: TreeGrammar, pattern: Muster): TreeGrammar = {
    tys = (grammar.map(_._1.toString())).toSet
    var nameProvider = FreshNameProvider(pushName, None, tys)
    val (changed, nextGrammar) =
      grammar.foldLeft((false, emptyGrammar)) { case ((hasChanged, nextGrammar), (n, rhss)) =>
        val (newEntries, matched) = forbidIn(grammar, pattern, n, rhss, nameProvider)
        ((hasChanged || matched), nextGrammar ++ newEntries)
      }
    if (changed) {
      forbid(nextGrammar, pattern) }
    else { nextGrammar }
  }

  def forbidIn(grammar: TreeGrammar, pattern: Muster, n: Type, rhss: Set[(String, Seq[Type])], nameProvider: FreshNameProvider[String]): (TreeGrammar, Boolean) = {
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[Type])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(na, pats) if na == combinator && pats.size == args.size  =>
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[Type], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                    val newArg = nameProvider.freshNameBasedOn(arg.toString())
                    val (newGrammar, matchedRec) = forbidIn(grammar, pat, Constructor(newArg._1), grammar(arg), newArg._2)
                    ((leftArgs :+ arg, rightArgs.tail),
                      newRhss + ((combinator -> (leftArgs ++ (Constructor(newArg._1) +: rightArgs.tail)))),
                      additionalGrammar ++ newGrammar,
                      matched && matchedRec)
                }
              if (nextMatched) {
                (nextRhss, nextAdditional, matched || nextMatched)
              } else {
                (newRhss + (combinator -> args), additionalGrammar, matched)
              }
            case Star() => (newRhss, additionalGrammar, true)
            case _ =>
              (newRhss + (combinator -> args), additionalGrammar, matched)
          }
      }
    (additionalGrammar + (n -> newRhss), matched)
  }

}

