package org.combinators.cls.ide.translator

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Omega, Type}

class ApplicativeTreeGrammarToTreeGrammar {

  val emptyGrammar: TreeGrammar = Map.empty
  var functionTypes: Set[Type] = Set.empty

  def translateATGtoTG(applicativeTreeGrammar: Set[Rule]): TreeGrammar = {
    var treeGrammar: TreeGrammar = emptyGrammar
    functionTypes = Set.empty
    applicativeTreeGrammar.foreach {
      case Combinator(target, combinator) =>
        treeGrammar = updateMap(treeGrammar, target, (combinator, Seq[Type]()))
      case Apply(nt, funct, atgt) =>
        val (str, argsList) = computeRules(applicativeTreeGrammar, funct)
        val rule = treeGrammar.find(_._1.equals(nt))
        if (argsList.contains(Constructor("*"))) {
          treeGrammar = treeGrammar + (nt -> Set(("*", Seq.empty)))
        } else {
          rule match {
            case Some(_) =>
              if (str.size == 1) {
                treeGrammar = updateMap(treeGrammar, nt, ((str.head, argsList :+ atgt)))
              } else {
                str.foreach(e => treeGrammar = updateMap(treeGrammar, nt, ((e, argsList :+ atgt))))
              }
            case None =>
              if (argsList.nonEmpty) {
                val newEntry = (nt -> Set((str.head, argsList :+ atgt)))
                treeGrammar = treeGrammar + newEntry
              } else {
                if (str.size == 1) {
                  treeGrammar = treeGrammar + (nt -> Set((str.head, argsList :+ atgt)))
                } else {
                  str.foreach(e => treeGrammar = updateMap(treeGrammar, nt, ((e, argsList :+ atgt))))
                }
              }
          }
        }
      case Failed(_) => treeGrammar
    }
    treeGrammar = removeFreshNT(treeGrammar)
    treeGrammar
  }

  def removeFreshNT(grammar: TreeGrammar): TreeGrammar = {
    grammar.filterNot(e => functionTypes.contains(e._1))
  }

  def updateMap(map: TreeGrammar, key: Type, value: (String, Seq[Type])) = {
    map + ((key, map.getOrElse(key, Set()) + value))
  }


  def computeRules(appTG: Set[Rule], funcType: Type): (Seq[String], (Seq[Type])) = {
    var argsList: Seq[Type] = Seq.empty
    var comb: Seq[String] = Seq.empty
    functionTypes = functionTypes + funcType
    appTG.foreach(rule =>
      if (rule.target.equals(funcType)) rule match {
        case Apply(nt, fType, argType) =>
          if (fType.equals(Omega)) {
            argsList = Seq(Constructor("*"))
          } else {
            val (str, args) = computeRules(appTG, fType)
            argsList = argsList ++ args :+ argType
            comb = comb ++ str
          }
        case Combinator(tgt, combinator) =>
          if (tgt.equals(funcType)) {
            comb = comb :+ combinator
          }
        case Failed(_) =>
      })
    (comb, argsList)
  }
}
