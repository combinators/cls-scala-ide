
package org.combinators.cls.ide.filter

import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.types.{Constructor, Type}

object ForbidInListTest extends App {
  val filter = new FilterList
  type NT = Type

  /*  sealed trait Pattern
    final case class Star() extends Pattern
    final case class Term(name: String, args: Seq[Pattern]) extends Pattern*/

  val grammar: TreeGrammar =
    Map[Type, Set[(String, Seq[Type])]](
      Constructor("sigma0") -> Set(
        ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
        ("g", Seq[Type](Constructor("sigma3")))),
      Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
      Constructor("sigma2") -> Set(("c3", Seq.empty)),
      Constructor("sigma3") ->
        Set(
          ("c4", Seq.empty[Type]),
          ("c5", Seq[Type](Constructor("sigma0")))
        )
    )

  val testGrammar: TreeGrammar =
    Map(
      Constructor("A") -> Set(("c", Seq[Type](Constructor("D"), Constructor("E"))), ("c", Seq[Type](Constructor("D"))), ("c", Seq[Type](Constructor("D"), Constructor("F"))), ("f", Seq[Type](Constructor("D"), Constructor("E")))),
      Constructor("B") -> Set(("c", Seq[Type](Constructor("D"), Constructor("E")))),
      Constructor("D") -> Set(("d", Seq[Type](Constructor("B"))), ("d", Seq[Type]()), ("x", Seq[Type](Constructor("A")))),
      Constructor("E") -> Set(("e", Seq[Type]())),
      Constructor("F") -> Set(("y", Seq[Type](Constructor("B"))), ("d", Seq[Type]()))
    )

  //val testPattern = Term("c", Seq(Term("d", Seq.empty), Term("e", Seq.empty)))
  val testPattern = Term("c", Seq(Term("d", Seq(Term("m", Seq.empty))), Term("e", Seq.empty)))
  //val testPattern = Term("c", Seq(Term("d", Seq(Term("c", Seq(Term("d", Seq(Star())), Star())))), Term("e", Seq.empty)))
  val testPattern1 = Term("c", Seq(Term("d", Seq(Star())), Term("e", Seq.empty)))


  //val muster1: Muster = Term("f", Seq(Term("c2", Seq.empty), Term("c3", Seq.empty)))

  val emptyGrammar: TreeGrammar = Map.empty
  def forbid(grammar: TreeGrammar, pattern: Muster): TreeGrammar = {
    val (changed, nextGrammar) =
      grammar.foldLeft((false, emptyGrammar)) { case ((hasChanged, nextGrammar), (n, rhss)) =>
        val (newEntries, matched) = forbidIn(grammar, pattern, n, rhss)
        ((hasChanged || matched), nextGrammar ++ newEntries)
      }
    if (changed) {
      forbid(nextGrammar, pattern) }
    else {
      nextGrammar }
  }
  def computeNewGrammar(grammar: TreeGrammar, pattern: Muster): TreeGrammar= {
    val newList =
      grammar.foldLeft(emptyGrammar) {
        case (newList, (_, rhss)) =>
          val additionalGrammar = rhss.foldLeft(emptyGrammar) {
            case (additionalGrammar, (combinator, args)) =>
              pattern match {
                case Term (c, pats) if (c == combinator && pats.size == args.size) =>
                  val newGram = args.foldLeft(additionalGrammar) {
                    case (additionalGrammar, arg) =>
                      (additionalGrammar + (arg -> grammar(arg)))
                  }
                  val recBla = pats.foldLeft(additionalGrammar){
                    case (additionalGrammar, pat) =>
                      additionalGrammar++computeNewGrammar(grammar, pat)
                  }
                  additionalGrammar++newGram++recBla
                case Star() => additionalGrammar
                case _ => additionalGrammar
              }
          }
          newList++ additionalGrammar
      }
    newList
  }

  def forbidNew(grammar: TreeGrammar, pattern: Muster): TreeGrammar = {
    grammar.foreach { case (n, rhss) =>
      println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")}
    println("-----")
    val newLists = computeNewGrammar(grammar, pattern)
    println("xxx", newLists)
    /*  val newList =
      grammar.foldLeft(emptyGrammar) {
          case (newList, (_, rhss)) =>
            val additionalGrammar = computeNewGrammar(grammar, pattern)
            newList ++ additionalGrammar
        }*/
    val (changed, nextGrammar) =
      newLists.foldLeft((false, emptyGrammar)) { case ((hasChanged, nextGrammar), (n, rhss)) =>
        val (newEntries, matched) = forbidIn(newLists, pattern, n, rhss)
        ((hasChanged || matched), nextGrammar ++ newEntries)
      }
    val probegr = nextGrammar++grammar
    /* probegr.foreach { case (n, rhss) =>
      println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")}
    println("-----")
   val newGr = probegr.foldLeft(emptyGrammar) { case (nextGrammar, (n, rhss)) =>
      val newEntries = forbidIn(probegr, pattern, n, rhss)
      (nextGrammar ++ newEntries._1)
    }*/
    val newGr = forbid(probegr, pattern)
    /*newGr.foreach { case (n, rhss) =>
      println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")}
    println("xxxx")*/
    newGr
  }
  /* println("sss", subForbidList)
   if (changed) {
     println("orgGrammarSize", grammar.size)
     println("nextGrammarSize", nextGrammar.size)
     //println("xxxxx", nextGrammar)
     forbid(nextGrammar, pattern)
   }
   else {
     println("----------", "end")
     nextGrammar
   }*/


  var counter = 0

  /*def forbidInList(grammar: TreeGrammar, args: Seq[NT], pattern: Seq[Muster]): TreeGrammar = {
    val subForbidList = args.foldLeft(emptyGrammar) {
      case (subForbid, arg) =>
        subForbid + (arg -> grammar(arg))
        }
    println("----", subForbidList)
    subForbidList
  }*/

  def forbidIn(grammar: TreeGrammar, pattern: Muster, n: Type, rhss: Set[(String, Seq[NT])]): (TreeGrammar, Boolean) = {
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[NT])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(c, pats) if c == combinator && (pats.size == args.size) =>
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[NT], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                    val newArg = arg.toString() + counter
                    counter += 1
                    val (newGrammar, matchedRec) = forbidIn(grammar, pat, Constructor(newArg), grammar(arg))
                    // println("---", arg, grammar(arg))
                    ((leftArgs :+ arg, rightArgs.tail),
                      newRhss + ((combinator -> (leftArgs ++ (Constructor(newArg) +: rightArgs.tail)))),
                      additionalGrammar ++ newGrammar,
                      matched && matchedRec
                    )
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
    (additionalGrammar + (n -> newRhss), matched)
  }

  //val postprocessed = forbidNew(testGrammar,
  //  testPattern)
  val postprocessed1 = filter.forbid(testGrammar,
    testPattern)
  postprocessed1.foreach { case (n, rhss) =>
    println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")
  }
}




