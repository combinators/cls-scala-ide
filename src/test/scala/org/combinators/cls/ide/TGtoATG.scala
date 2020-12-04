package org.combinators.cls.ide

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.ide.filter.FreshNameProvider
import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Arrow, Constructor, SubtypeEnvironment, Type}

import scala.annotation.tailrec

object TGtoATG extends App {
  type NT = String
  //type TreeGrammar = Map[NT, Set[(String, Seq[NT])]]

  var counter = 0

  var rules: Set[Rule] = Set.empty

  sealed trait Rule  {
    val target: Type
  }
  final case class Failed(target: Type) extends Rule

  /** Represents types inhabited by a combinator without arguments. */
  final case class Combinator(target: Type, combinator: String) extends Rule

  /** Represents the application of a term of type `functionType` to a term of type `argumentType` to obtain
    * type `target`. */
  final case class Apply(target: Type, functionType: Type, argumentType: Type)
    extends Rule

  sealed trait Pattern
  final case class Star() extends Pattern
  final case class Term(name: String, args: Seq[Pattern]) extends Pattern


  val testGrammar: TreeGrammar =
    Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
      ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
      ("f", Seq(Constructor("D"), Constructor("E")))),
  Constructor("B") -> Set(("c", Seq(Constructor("D"), Constructor("E")))),
  Constructor( "D") -> Set(("d", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq(Constructor("A")))),
  Constructor("E") -> Set(("e", Seq())),
  Constructor("F") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()))
    )

  val testGrammar3: TreeGrammar =
    Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"), Constructor("K"), Constructor("F"), Constructor("L"), Constructor("M"), Constructor("N"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))), ("f", Seq(Constructor("D"), Constructor("E")))),
        Constructor("B") -> Set(("c", Seq(Constructor("D"), Constructor("E")))),
          Constructor("D") -> Set(("d", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq(Constructor("A")))),
            Constructor("E") -> Set(("e", Seq())),
              Constructor("F") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()))
    )
  val repository =
    Map(
      "c" -> Arrow(Constructor("sigma1"), Arrow(Constructor("sigma2"), Constructor("sigma0"))),
      "g" -> Arrow(Constructor("sigma3"), Constructor("sigma0")),
      "e" -> Constructor("sigma1"),
      "c3" -> Arrow(Constructor("sigma3"), Constructor("sigma2")),
      "c4" -> Constructor("sigma3"),
      "c5" -> Arrow(Constructor("sigma0"), Constructor("sigma3"))
    )

  val A = Constructor("A")
  val B = Constructor("B")
  val C = Constructor("C")
  val D = Constructor("D")
  val E = Constructor("E")


  val applicativeTreeGrammar: Set[Rule] =
    Set[Rule](
      Combinator(A, "F"),
      Apply(A, Arrow(A, A), A),
      Apply(Arrow(A, A), Arrow(A, Arrow(A, A)), A),
      Combinator(Arrow(A, Arrow(A, A)), "G"),
      Apply(A, Arrow(B, A), B),
      Apply(Arrow(B, A), Arrow(A, Arrow(B, A)), A),
      Combinator(Arrow(A, Arrow(B, A)), "H"),
      Combinator(B, "I"),
      Apply(B, Arrow(A, B), A),
      Combinator(Arrow(A, B), "J"),
      Combinator(C, "K"),
      Failed(D),
      Combinator(E, "L"),
      Apply(E, Arrow(E, E), E),
      Combinator(Arrow(E, E), "M")
    )

 /* val testApplicativeGrammar: TreeGrammar =
    Map(
      "A" -> Set(("c", Seq("D", "E")), ("c", Seq.empty), ("c", Seq("D", "F")), ("f", Seq("D", "E"))),
      "B" -> Set(("c", Seq("D", "E"))),
      "D" -> Set(("d", Seq.empty), ("d", Seq()), ("x", Seq())),
      "E" -> Set(("e", Seq())),
      "F" -> Set(("y", Seq()), ("d", Seq()))
    )
*/



  val emptyGrammar: TreeGrammar = Map.empty

  def isApplicativeTG(treeGrammar: TreeGrammar): Boolean = {
    var isATG = true
    //println("sss ", treeGrammar)
    //println("iiii ", isATG)
    treeGrammar.map(e => e._2.map(k => if (k._2.nonEmpty && !k._2.size.equals(2)) {
     // println("false", k._2.size.equals(0))
      isATG = false
    } ))
    isATG
  }

  var argsList: Seq[Type] = Seq.empty
  var treeGrammar: TreeGrammar = emptyGrammar
  def translateATGtoTG(applicativeTreeGrammar:Set[Rule]): TreeGrammar = {
    applicativeTreeGrammar.foreach {
      case Combinator(target, combinator) =>
    //    treeGrammar = treeGrammar + (target -> Set((combinator, Seq[Type]())))
        treeGrammar = updateMap(treeGrammar, target,  (combinator, Seq[Type]()))
      case Apply(nt, funct, atgt) =>
        val args: Seq[Type] = Seq.empty
        args :+ atgt.toString()
        val newArg = computeRules(applicativeTreeGrammar, funct, args)
        treeGrammar.find(_._1.equals(nt)) match {
          case Some(xs) =>
           treeGrammar = updateMap(treeGrammar, nt,  ((argsList.head.toString(), argsList.drop(1) :+ atgt)))
            println("----", treeGrammar)
          case None =>
            treeGrammar = treeGrammar + (nt -> Set((argsList.head.toString(), argsList.drop(1) :+ atgt)))

        }
       argsList = Seq.empty
    }
    treeGrammar = removeFreshNT(treeGrammar)
    treeGrammar
  }

  def removeFreshNT(grammar: TreeGrammar): TreeGrammar= {
    grammar.filterNot(_._1.toString().contains("'"))
  }


  def updateMap(map: TreeGrammar, key: Type, value:(String, Seq[Type])) = {
    map + ((key, map.getOrElse(key, Set()) + value))}

  def computeRules(appTG: Set[Rule], funcType: Type, aktArgs: Seq[Type]): String = {

    var arg: String = ""
    appTG.foreach {
      case Apply(nt, fType, argType) =>
        if (nt.equals(funcType)) {

          println("hallo", nt, fType, argType )
          val newArgs = computeRules(appTG, fType, aktArgs)
          argsList = argsList :+ argType
          treeGrammar + (nt -> ((newArgs, aktArgs)))
          arg = argType.toString()
          arg
        }
        else {
          arg = funcType.toString()
          arg
        }
      case Combinator(target, combinator) =>
        if (target.equals(funcType)) {
          argsList = argsList :+ Constructor(combinator)
          arg = combinator
        }
        else {
          arg = funcType.toString()
          arg
        }
    }
    arg
  }
  var tys: Set[String] = Set.empty
  var nameProvider = FreshNameProvider(pushName, None, tys)
  def translateTGtoATG(grammar: TreeGrammar): Set[Rule] = {
    var rules: Set[Rule] = Set.empty
    tys = (grammar.map(_._1.toString())).toSet ++ grammar.flatMap(_._2.map(_._1)).toSet
    grammar.map {
      case (ty, args) => args.map(k =>
        rules = rules ++ computeFunctionTypes(k._2, ty, k._1)
      )
    }
    rules
  }


  def computeFunctionTypes(args: Seq[Type], ty: Type, combinator: String): Set[Rule]= {
    var argList = args
    var newFT= ty.toString()
    nameProvider = FreshNameProvider(pushName, None, tys)
    if(argList.size > 1){
      newFT = nameProvider.freshNameBasedOn(newFT)._1
      tys = tys + newFT
     // newFT = newFT.toString()+counter
      rules = rules + Apply(ty, Constructor(newFT), argList.reverse.head)
      argList = argList.dropRight(1)
     // counter+=1
      computeFunctionTypes(argList, Constructor(newFT), combinator)
    }else{
      if (argList.isEmpty) {
        rules = rules + Combinator(ty, combinator)
      }else{
        val newCombinatorName = nameProvider.freshNameBasedOn(combinator)
        tys = tys + newCombinatorName._1
        rules = rules + Apply(Constructor(newFT), Constructor(newCombinatorName._1), argList.head)
        //rules = rules + Apply(Constructor(newFT), Constructor(combinator+counter), argList.head)
          rules = rules + Combinator(Constructor(newCombinatorName._1), combinator)
      //  rules = rules + Combinator(Constructor(combinator+counter), combinator)
      }
     }
   rules
  }

  def prettyPrintRuleSet(rules: Set[Rule]): String = {
    rules
      .groupBy(_.target)
      .map {
        case (target, entries) =>
          val prettyEntries = entries.map {
            case Failed(_)                 => "Uninhabited"
            case Combinator(_, combinator) => combinator
            case Apply(_, functionType, argumentType) =>
              s"@($functionType, $argumentType)"
          }
          s"$target --> ${prettyEntries.mkString(" | ")}"
      }
      .mkString("{", "; ", "}")
  }



  @tailrec
  def forbid(grammar: TreeGrammar, pattern: Pattern): TreeGrammar = {
    val (changed, nextGrammar) =
      grammar.foldLeft((false, emptyGrammar)) { case ((hasChanged, nextGrammar), (n, rhss)) =>
        val (newEntries, matched) = forbidIn(grammar, pattern, n, rhss)
        (hasChanged || matched, nextGrammar ++ newEntries)
      }
    if (changed) {
      forbid(nextGrammar, pattern) }
    else {
      nextGrammar }
  }



  def forbidIn(grammar: TreeGrammar, pattern: Pattern, n: Type, rhss: Set[(String, Seq[Type])]): (TreeGrammar, Boolean) = {
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[Type])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(n, pats) if n == combinator && pats.size == args.size  =>
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[Type], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                    val newArg = arg.toString() + counter
                    counter += 1
                    val (newGrammar, matchedRec) = forbidIn(grammar, pat, Constructor(newArg), grammar(arg))
                    ((leftArgs :+ arg, rightArgs.tail),
                      newRhss + (combinator -> (leftArgs ++ (Constructor(newArg) +: rightArgs.tail))),
                      additionalGrammar ++ newGrammar,
                      matched && matchedRec)
                }
              if (nextMatched) {
                (nextRhss, nextAdditional, matched || nextMatched)
              } else {
                (newRhss + (combinator -> args), additionalGrammar, matched)
              }
            case Star() =>
              (newRhss, additionalGrammar, true)
            case _ =>
              (newRhss + (combinator -> args), additionalGrammar, matched)
          }
      }
    (additionalGrammar + (n -> newRhss), matched)
  }


  lazy val tree = GammaAssociation.inhabit(Constructor("sigma3"))
  //val t1 = System.nanoTime()
  // val postprocessed = forbid(testGrammar, testPattern1)
  //val duration = (System.nanoTime() - t1)

  println("---------", prettyPrintRuleSet(applicativeTreeGrammar))
  val isATree = isApplicativeTG(testGrammar)
  val isAppTree = translateTGtoATG(testGrammar3)
  val isTG = translateATGtoTG(applicativeTreeGrammar)
  val isTree = translateATGtoTG(isAppTree)

  //println("---------", isATree)
 // println("tttt", isTG)

  println("---------", isTG)
 lazy val testChannel = new DebugMsgChannel()

  lazy val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)
  val tgtAssociation = Constructor("X")
  val tgtPat = Constructor("p! X")
 // println("<<<<<<<", isTree)
  val prune = GammaAssociation.prune(isTree, Set(Constructor("A")))
  val resultTerm = InhabitationResult[Unit](prune, tgtPat, x => ())

  //println(">>>>>>>")
  //println("<<<<<<<", prune)
  import java.util.concurrent.TimeUnit


  def pushName(name:String, index:Int = 3): String = {
    name + StringUtils.repeat("'", index)
  }
  /*println("time", duration.asInstanceOf[Double] / 1000000000.0)
  postprocessed.foreach { case (n, rhss) =>
    println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
  }*/
}