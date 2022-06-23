package org.combinators.cls.ide.filter

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, SubtypeEnvironment, Type}

// $COVERAGE-OFF$Disabling highlighting by default until a workaround for https://issues.scala-lang.org/browse/SI-8596 is found

class FilterNew {

  val testChannel = new DebugMsgChannel()
  val Gamma = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), Map())


  type Rule = (Type, Set[(String, Seq[Type])])
  var tys: Set[String] = Set.empty
  var nameProvider = FreshNameProvider(pushName, None, tys)
  def makeNewRules(treeGrammar: TreeGrammar, muster: Muster): TreeGrammar = {
    tys = (treeGrammar.map(_._1.toString())).toSet
    nameProvider = FreshNameProvider(pushName, None, tys)
    val (changed, nextGrammar): (Boolean, TreeGrammar) = treeGrammar.foldLeft((false, Map[Type, Set[(String, Seq[Type])]]())) {

      case ((hasChanged, grammar), (n, rhss)) =>
        val newEntries: TreeGrammar = forbidIn(treeGrammar, muster, n, rhss, tys, nameProvider)

        (hasChanged || newEntries != Map(n -> rhss), grammar ++ newEntries.toSeq)
    }
    if (changed) {
      val prune = Gamma.prune(nextGrammar)
      makeNewRules(nextGrammar, muster)
    }
    else {

      nextGrammar
    }
  }

  def forbidIn(treeGrammar: TreeGrammar,
               muster: Muster,
               nonterminal: Type,
               rhss: Set[(String, Seq[Type])],
               tys: Set[String],
               nameProvider: FreshNameProvider[String]): TreeGrammar = {
    var must: String = ""
    val (newRhss, additionalGrammar): (Set[(String, Seq[Type])], TreeGrammar) =
      rhss.foldLeft((Set[(String, Seq[Type])](), Map[Type, Set[(String, Seq[Type])]]())) {
        case ((newRhss, additionalGrammar), (combinator, args)) =>
          muster match {
            case Term(m, pats) if (m == combinator && pats.size == args.size) =>

              val (_, nextRhss, nextGrammar) =
                pats.zip(args).foldLeft(((Seq[Type](), args), newRhss, additionalGrammar)) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar), (pat, arg)) =>
                    val newArg = nameProvider.freshNameBasedOn(arg.toString())
                    val newTys = tys + newArg._1
                    must = arg.toString()

                    val rhs = treeGrammar.find(e => e._1 == arg) match {
                      case Some(value) => forbidIn(treeGrammar, pat, Constructor(newArg._1), value._2, newTys, newArg._2)
                      case None => Map[Type, Set[(String, Seq[Type])]]()
                    }
                    ((leftArgs :+ arg, rightArgs.tail),
                      newRhss ++ Set((combinator, leftArgs ++ (Constructor(newArg._1) +: rightArgs.tail))),
                      additionalGrammar ++ rhs)


                }
              (nextRhss, nextGrammar)
            case Star() => (newRhss, additionalGrammar)
            // wenn der Muster nicht zu der rechten Seite passt
            case _ => (newRhss ++ Set((combinator, args)), additionalGrammar)
          }
      }
    additionalGrammar ++ Map(nonterminal -> newRhss)

  }

  def pushName(name:String, index:Int = 3): String = {
    name + StringUtils.repeat("'", index)
  }


}
