package org.combinators.cls.ide

import org.apache.commons.lang3.StringUtils
import org.combinators.cls.ide.filter.FreshNameProvider
import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.ide.translator.{Apply, Combinator, Failed, Rule}
import org.combinators.cls.ide.translator.ApplicativeTreeGrammarToTreeGrammar
import org.combinators.cls.ide.translator.TreeGrammarToApplivativeTreeGrammar
import org.combinators.cls.inhabitation.TreeGrammar
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Arrow, Constructor, Intersection, Omega, SubtypeEnvironment, Type, Variable}
import org.scalatest.FunSpec
import scala.annotation.tailrec

class TraslatorTest extends FunSpec {

  val traslatorToApplicativeTreeGrammar = new TreeGrammarToApplivativeTreeGrammar()
  val traslatorToTreeGrammar = new ApplicativeTreeGrammarToTreeGrammar()

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
  lazy val isAppTree = traslatorToApplicativeTreeGrammar.translateTGtoATG(testGrammar)
  lazy val isAppTree3 = traslatorToApplicativeTreeGrammar.translateTGtoATG(testGrammar3)
  //val isTG = translateATGtoTG(applicativeTreeGrammar)

  lazy val isTree = traslatorToTreeGrammar.translateATGtoTG(isAppTree)
  lazy val isTree3 = traslatorToTreeGrammar.translateATGtoTG(isAppTree3)


  describe("testGrammar3 to applicative tree grammar to testGrammar3") {
    val treeGrammarRule3: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(
        ("c", Seq(Constructor("D"), Constructor("E"), Constructor("K"), Constructor("F"), Constructor("L"), Constructor("M"), Constructor("N"))),
        ("c", Seq(Constructor("D"))),
        ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E"))))
    )
    it("should not be empty1") {
      assert(isTree3.nonEmpty)
    }
    it("should be equal to testGrammar3") {
      assert(isTree3.equals(testGrammar3))
    }
    it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E, K, F, L, M, N))) }"
    ) {
      treeGrammarRule3.toSet.subsetOf(isTree3.toSet)
    }
  }
  describe("testGrammar to applicative tree grammar to testGrammar") {

    val treeGrammarRule: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E")))),
    )
    it("should not be empty") {
      assert(isTree.nonEmpty)
    }
    it("should be equal to testTreeGrammar") {
      assert(isTree.equals(testGrammar))
    }
    it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E))) }"
    ) {
      treeGrammarRule.toSet.subsetOf(isTree.toSet)
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
    /*it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E))) }"
    ) {}*/
  }
  describe("applicative tree grammar with omega to tree Grammar") {
    lazy val isTree3a = traslatorToTreeGrammar.translateATGtoTG(edgeCaseTree)

    it("should not be empty3") {
      assert(isAppTree.nonEmpty)
    }
    it("should be equal to testGrammar3a") {
     // assert(isTree3.equals(testGrammar))
    }
    /*it(
      "should include {  A |-> Set((c,List(D)), (f,List(D, E)), (c,List(D, F)), (c,List(D, E))) }"
    ) {}*/
  }

  lazy val tree = GammaAssociation.inhabit(Constructor("sigma3"))
  //val t1 = System.nanoTime()
  // val postprocessed = forbid(testGrammar, testPattern1)
  //val duration = (System.nanoTime() - t1)

  println("--------a", prettyPrintRuleSet(edgeCaseTree))

  //println("---------", isATree)
  // println("tttt", isTG)

  println("tree", testGrammar)
  println("--------a", prettyPrintRuleSet(isAppTree))
  //println("---------", isTree)
  lazy val testChannel = new DebugMsgChannel()

  lazy val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)

}