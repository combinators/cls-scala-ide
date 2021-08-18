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
      case Apply(nt, funcType, atgType) =>
        val res = computeRules(applicativeTreeGrammar, funcType, Seq(atgType), Seq.empty)
        val rule = treeGrammar.find(_._1.equals(nt))
        if(res.nonEmpty){
        if ( res.head._2.contains(Constructor("*"))) {
          treeGrammar = treeGrammar + (nt -> Set(("*", Seq.empty)))
        } else {
          rule match {
            case Some(_) =>
              res.foreach(r => treeGrammar = updateMap(treeGrammar, nt, r))

            case None =>
                val newEntry = nt -> Set((res.head._1, res.head._2))
                treeGrammar = treeGrammar + newEntry
          }
        }}
      case Failed(_) => treeGrammar
    }
    treeGrammar
  }

  def removeFreshNT(grammar: TreeGrammar): TreeGrammar = {
    grammar.filterNot(e => functionTypes.contains(e._1))
  }

  def updateMap(map: TreeGrammar, key: Type, value: (String, Seq[Type])): Map[Type, Set[(String, Seq[Type])]] = {
    map + ((key, map.getOrElse(key, Set()) + value))
  }


  def computeRules(appTG: Set[Rule], funcType: Type, actArgs:Seq[Type], allAkt:Seq[(String, Seq[Type])]): Seq[(String, Seq[Type])] = {
    var argsUpdate: Seq[Type] = actArgs
    var all: Seq[(String, Seq[Type])] = allAkt
    var argsList: Seq[Type] = Seq.empty
    var comb: Seq[String] = Seq.empty
    functionTypes = functionTypes + funcType
    val filter = appTG.filter(_.target.equals(funcType))
    if(filter.size==2){
    }
    filter.foreach(rule =>
      rule match {
        case Apply(_, fType, argType) =>
          if (fType.equals(Omega)) {
            argsList = Seq(Constructor("*"))
          } else {
            if(all.isEmpty){
            argsUpdate = argType +: argsUpdate
            all = all ++ computeRules(appTG, fType, argsUpdate, all)
            }
            else{
              val newArgs = argType +: actArgs
              all = all ++ computeRules(appTG, fType, newArgs, all)
            }
          }
        case Combinator(_, combinator) =>
          all = all :+ ((combinator, argsUpdate))
        case Failed(_) =>
      })
    all
  }
}
