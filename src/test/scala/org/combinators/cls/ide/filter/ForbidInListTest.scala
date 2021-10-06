
package org.combinators.cls.ide.filter

import org.combinators.cls.inhabitation.{Tree, TreeGrammar}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Constructor, Type}

import java.io.{BufferedWriter, File, FileWriter}

object ForbidInListTest extends App {
  val filter = new FilterList
  val filter2 = new FilterRec
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

  val treeGrammar: Map[Type, Set[(String, Seq[Type])]] = Map(
    Constructor("S") -> Set(
      ("c", Seq(Constructor("A"), Constructor("B"))),
      ("c2", Seq.empty)),
    Constructor("A") -> Set(
      ("c", Seq(Constructor("A"), Constructor("B"))),
      ("c1", Seq.empty)),
    Constructor("B") -> Set(
      ("c2", Seq.empty))
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
  val testPatternAsso = Term("c", Seq(Term("c", Seq(Star(), Star())), Star()))
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
    val newLists = computeNewGrammar(grammar, pattern)

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
    val newGr = forbid(probegr, pattern)
    newGr
  }


  var counter = 0


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
    //println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")
  }

  val postprocessed2 = filter2.forbid(treeGrammar,
    testPatternAsso)
  println("wwwwwwwwwwwwwwwwwwwwwwwwwww", testPatternAsso)
  postprocessed2.foreach { case (n, rhss) =>
    println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|")}")
  }

  val results = InhabitationResult[Unit](postprocessed2, Constructor("S"), x => ())
  //val resultsOrg = InhabitationResult[Unit](treeGrammar, Constructor("S"), x => ())
  println(results.size)
  println(results.size, results.isInfinite)


  val pw2 =new BufferedWriter(new FileWriter(new File("orgREs.txt")))
  for (index <- 0 until 10){
    val terms3 = mkTreeMap(Seq(results.terms.index(index)))

    println(index,": ", results.terms.index(index))
    pw2.write(terms3.toString())
    pw2.write("\n")

  }
  pw2.close()

  def mkTreeMap(trees: Seq[Tree]): Seq[Muster] = {
    var seqTree: Seq[Muster] = Seq.empty
    for (tree <- trees){
      seqTree = seqTree ++ Seq(tree match {
        case Tree(name, _, arguments@_*)=>
          Term(name, mkTreeMap(arguments))
        case _ => Star()
      })
    }
    seqTree
  }
}




