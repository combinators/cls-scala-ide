package org.combinators.cls.ide.filter

object TGPat extends App {
  type NT = String
  type TreeGrammar = Map[NT, Set[(String, Seq[NT])]]

  sealed trait Pattern
  final case class Star() extends Pattern
  final case class Term(name: String, args: Seq[Pattern]) extends Pattern


  val testGrammar: TreeGrammar =
    Map(
      "A" -> Set(("c", Seq("D", "E")), ("c", Seq("D")), ("c", Seq("D", "F")), ("f", Seq("D", "E"))),
      "B" -> Set(("c", Seq("D", "E"))),
      "D" -> Set(("d", Seq("B")), ("d", Seq()), ("x", Seq("A"))),
      "E" -> Set(("e", Seq())),
      "F" -> Set(("y", Seq("B")), ("d", Seq()))
    )

  val testPattern = Term("c", Seq(Term("d", Seq(Star())), Term("e", Seq.empty)))
  val testPattern1 = Term("c", Seq(Term("d", Seq(Term("c", Seq(Term("d", Seq(Star())), Star())))), Term("e", Seq.empty)))


  val emptyGrammar: TreeGrammar = Map.empty

  def forbid(grammar: TreeGrammar, pattern: Pattern): TreeGrammar = {
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

  var counter = 0

  def forbidIn(grammar: TreeGrammar, pattern: Pattern, n: NT, rhss: Set[(String, Seq[NT])]): (TreeGrammar, Boolean) = {
    val (newRhss, additionalGrammar, matched) =
      rhss.foldLeft((Set.empty[(String, Seq[NT])], emptyGrammar, false)) {
        case ((newRhss, additionalGrammar, matched), (combinator, args)) =>
          pattern match {
            case Term(n, pats) if n == combinator && pats.size == args.size  =>
              val (_, nextRhss, nextAdditional, nextMatched) =
                pats.zip(args).foldLeft((Seq.empty[NT], args), newRhss, additionalGrammar, true) {
                  case (((leftArgs, rightArgs), newRhss, additionalGrammar, matched), (pat, arg)) =>
                    val newArg = arg + counter
                    counter += 1
                    val (newGrammar, matchedRec) = forbidIn(grammar, pat, newArg, grammar(arg))
                    ((leftArgs :+ arg, rightArgs.tail),
                      newRhss + ((combinator -> (leftArgs ++ (newArg +: rightArgs.tail)))),
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
  val t1 = System.nanoTime()
  val postprocessed = forbid(testGrammar, testPattern1)
  val duration = (System.nanoTime() - t1)

  import java.util.concurrent.TimeUnit

  println("time", duration.asInstanceOf[Double] / 1000000000.0)
  postprocessed.foreach { case (n, rhss) =>
    println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
  }
}

