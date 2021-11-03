package org.combinators.cls.ide

import org.combinators.cls.ide.translator._
import org.combinators.cls.inhabitation.{TreeGrammar, prettyPrintTreeGrammar}
import org.combinators.cls.types._
import org.scalatest.FunSpec

class TranslatorTest extends FunSpec {

  val translatorToApplicativeTreeGrammar = new TreeGrammarToApplicativeTreeGrammar()
  val translatorToTreeGrammar = new ApplicativeTreeGrammarToTreeGrammar()

  val testGrammar: TreeGrammar =
    Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E")))),
      Constructor("B") -> Set(("c", Seq(Constructor("D"), Constructor("E")))),
      Constructor("D") -> Set(("d", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq(Constructor("A")))),
      Constructor("E") -> Set(("e", Seq())),
      Constructor("F") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()))
    )
  val testGrammarStar: TreeGrammar =
    Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E"))),
        ("*", Seq())),
      Constructor("B") -> Set(("c", Seq(Constructor("D"), Constructor("E")))),
      Constructor("D") -> Set(("d", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq(Constructor("A")))),
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

  val A: Constructor = Constructor("A")
  val B: Constructor = Constructor("B")
  val C: Constructor = Constructor("C")
  val D: Constructor = Constructor("D")
  val E: Constructor = Constructor("E")

  val edgeCaseTree: Set[Rule] =
    Set[Rule](
      Apply(Constructor("X", Constructor("Int")), Arrow(Constructor("X", Constructor("Int")), Constructor("X", Constructor("Int"))), Constructor("X", Constructor("Int"))),
      Combinator(Omega, "gt"),
      Apply(Constructor("X", Constructor("Int")), Arrow(Arrow(Constructor("Int"), Constructor("Int")), Constructor("X", Constructor("Int"))), Arrow(Constructor("Int"), Constructor("Int"))),
      Combinator(Arrow(Constructor("X", Constructor("Int")), Constructor("X", Constructor("Int"))), "ia"),
      Combinator(Arrow(Omega, Constructor("X", Constructor("Int"))), "g"),
      Combinator(Omega, "ia"),
      Combinator(Omega, "f"),
      Combinator(Omega, "x"),
      Combinator(Omega, "gtArg"),
      Apply(Omega, Omega, Omega),
      Combinator(Arrow(Arrow(Constructor("Int"), Constructor("Int")), Constructor("X", Constructor("Int"))), "i"),
      Combinator(Arrow(Constructor("Int"), Constructor("Int")), "ia"),
      Combinator(Omega, "g"),
      Apply(Constructor("X", Constructor("Int")), Arrow(Omega, Constructor("X", Constructor("Int"))), Omega),
      Combinator(Omega, "z"),
      Combinator(Omega, "i"),
      Combinator(Constructor("X", Constructor("Int")), "x"),
      Combinator(Omega, "y")
    )

  val edgeCaseRepository =
    Map(
      "f" ->
        Arrow(
          Arrow(Variable("alpha"), Variable("beta")),
          Arrow(
            Constructor("X", Variable("alpha")),
            Constructor("Y", Variable("beta"))
          )
        ),
      "g" -> Arrow(Omega, Constructor("X", Variable("alpha"))),
      "x" -> Constructor("X", Constructor("Int")),
      "y" ->
        Arrow(
          Constructor("X", Variable("alpha")),
          Constructor("Y", Variable("alpha"))
        ),
      "z" ->
        Arrow(
          Constructor("X", Constructor("Bottom")),
          Constructor("Y", Constructor("Int"))
        ),
      "i" ->
        Intersection(
          Arrow(
            Arrow(Constructor("Int"), Constructor("Int")),
            Constructor("X", Constructor("Int"))
          ),
          Arrow(
            Arrow(Constructor("Char"), Constructor("Char")),
            Constructor("X", Constructor("String"))
          )
        ),
      "ia" -> Intersection(
        Arrow(Variable("alpha"), Variable("alpha")),
        Variable("gamma")
      ),
      "gt" -> Arrow(
        Intersection(
          Constructor("A"),
          Intersection(Constructor("C"), Constructor("B"))
        ),
        Arrow(
          Intersection(Constructor("B"), Constructor("A")),
          Constructor("Y", Constructor("Int"))
        )
      ),
      "gtArg" -> Intersection(
        Arrow(Constructor("X", Constructor("Int")), Constructor("A")),
        Intersection(
          Arrow(Constructor("Z", Constructor("Int")), Constructor("B")),
          Intersection(
            Arrow(Constructor("Z", Constructor("Int")), Constructor("C")),
            Intersection(
              Arrow(
                Constructor("Z", Constructor("Bottom")),
                Intersection(Constructor("A"), Constructor("B"))
              ),
              Arrow(
                Constructor("Y", Constructor("Int")),
                Arrow(
                  Constructor("OtherBottom"),
                  Arrow(
                    Constructor("Y", Constructor("OtherBottom")),
                    Intersection(Constructor("A"), Constructor("B"))
                  )
                )
              )
            )
          )
        )
      )
    )


  val applicativeTreeGrammarTest: Set[Rule] =
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


  def isApplicativeTG(treeGrammar: TreeGrammar): Boolean = {
    var isATG = true
    treeGrammar.map(e => e._2.map(k => if (k._2.nonEmpty && !k._2.size.equals(2)) {
      isATG = false
    }))
    isATG
  }

  def prettyPrintRuleSet(rules: Set[Rule]): String = {
    rules
      .groupBy(_.target)
      .map {
        case (target, entries) =>
          val prettyEntries = entries.map {
            case Failed(_) => "Uninhabited"
            case Combinator(_, combinator) => combinator
            case Apply(_, functionType, argumentType) =>
              s"@($functionType, $argumentType)"
          }
          s"$target --> ${prettyEntries.mkString(" | ")}"
      }
      .mkString("{", "; ", "}")
  }

  // val isTreeApp = translateATGtoTG(applicativeTreeGrammarTest)
  // val isTreeApp2 = translateATGtoTG(edgeCaseTree)
  //val isATree = isApplicativeTG(testGrammar3)
  lazy val isAppTree: Set[Rule] = translatorToApplicativeTreeGrammar.translateTGtoATG(testGrammar)
  lazy val isAppTreeStar: Set[Rule] = translatorToApplicativeTreeGrammar.translateTGtoATG(testGrammarStar)
  lazy val isAppTree3: Set[Rule] = translatorToApplicativeTreeGrammar.translateTGtoATG(testGrammar3)
  //val isTG = translateATGtoTG(applicativeTreeGrammar)

  lazy val isTree: TreeGrammar = translatorToTreeGrammar.translateATGtoTG(isAppTree)
  lazy val isTreeStar: TreeGrammar = translatorToTreeGrammar.translateATGtoTG(isAppTreeStar)
  lazy val isTree3: TreeGrammar = translatorToTreeGrammar.translateATGtoTG(isAppTree3)


  describe("testGrammar3 to applicative tree grammar to testGrammar3") {
    val isReducedTree = translatorToTreeGrammar.removeFreshNT(isTree3)
    val treeGrammarRule3: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(
        ("c", Seq(Constructor("D"), Constructor("E"), Constructor("K"), Constructor("F"), Constructor("L"), Constructor("M"), Constructor("N"))),
        ("c", Seq(Constructor("D"))),
        ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E"))))
    )
    it("should not be empty1") {
      assert(isReducedTree.nonEmpty)
    }
    it("should be equal to testGrammar3") {
      assert(isReducedTree.equals(testGrammar3))
    }
    it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E, K, F, L, M, N))) }"
    ) {
      treeGrammarRule3.toSet.subsetOf(isReducedTree.toSet)
    }
  }
  describe("testGrammar to applicative tree grammar to testGrammar") {

    val isReducedTree = translatorToTreeGrammar.removeFreshNT(isTree)
    val treeGrammarRule: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E")))),
    )
    it("should not be empty") {
      assert(isReducedTree.nonEmpty)
    }
    it("should be equal to testTreeGrammar") {
      assert(isReducedTree.equals(testGrammar))
    }
    it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E))) }"
    ) {
      treeGrammarRule.toSet.subsetOf(isReducedTree.toSet)
    }
  }
  describe("testGrammar with Star to applicative tree grammar to testGrammar") {
    val combinatorRules: Set[Rule] = Set(Combinator(Constructor("A"), "d"), Combinator(Constructor("A"), "e"))
    val applyRule: Set[Rule] = Set(Apply(Constructor("A"), Constructor("A"), Constructor("A")))

    it("should not be empty") {
      assert(isAppTreeStar.nonEmpty)
    }
    it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E))) }"
    ) {
      combinatorRules.subsetOf(isAppTreeStar)
    }
    it(
      "should include {  A |-> @(A, A) }"
    ) {
      applyRule.subsetOf(isAppTreeStar)
    }
  }

  describe("testGrammar to applicative tree grammar") {
    val setRules: Set[Rule] = Set(
      Combinator(Arrow(Constructor("B"), Constructor("D")), "d"),
      Apply(
        Arrow(Constructor("E"), Constructor("B")),
        Arrow(Arrow(Constructor("D"), Constructor("E")), Constructor("B")),
        Constructor("D")))
    it("should include { E -> B |-> @(D -> E -> B, D), B -> D |-> d }"
    ) {
      setRules.subsetOf(isAppTree)
    }
    it("should not be empty2") {
      assert(isAppTree.nonEmpty)
    }
  }
  describe("applicative tree grammar with omega to tree grammar") {
    lazy val isTree3a = translatorToTreeGrammar.translateATGtoTG(edgeCaseTree)
    val omegaRule:TreeGrammar = Map(Constructor("omega")-> Set(("*", Seq())) )
    it("should not be empty3") {
      assert(isAppTree.nonEmpty)
    }
    it("should include { omega -> *() }"
    ) {
      omegaRule.toSet.subsetOf(isTree3a.toSet)
    }
    describe("equality of applicative tree grammar with omega to tree grammar to applicative tree grammar") {
      lazy val isTree3a = translatorToTreeGrammar.translateATGtoTG(edgeCaseTree)
      lazy val isApplicativeTree = translatorToApplicativeTreeGrammar.translateTGtoATG(isTree3a)
      val omegaRule:TreeGrammar = Map(Constructor("omega")-> Set(("*", Seq())) )
      it("should not be empty3") {
        assert(isAppTree.nonEmpty)
      }
      it("should include { omega -> *() }"
      ) {
        omegaRule.toSet.subsetOf(isTree3a.toSet)
      }
      it("should be equal to T_ta(T_at(edgeCaseTree)) = edgeCaseTree}"
      ) {
        assert(isApplicativeTree.equals(edgeCaseTree))
      }
    }
  }



}