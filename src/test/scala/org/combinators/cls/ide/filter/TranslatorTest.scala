package org.combinators.cls.ide.filter

import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.ide.translator.{ApplicativeTreeGrammarToTreeGrammar, Apply, Combinator, Failed, Rule, TreeGrammarToApplicativeTreeGrammar}
import org.combinators.cls.inhabitation.{Tree, TreeGrammar}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Arrow, Constructor, Intersection, Omega, Product, SubtypeEnvironment, Type, Variable}
import org.scalatest.FunSpec

class TranslatorTest extends FunSpec {

    val filter = new FilterApply()
    val translator = new TreeGrammarToApplicativeTreeGrammar()
    val translatorBack = new ApplicativeTreeGrammarToTreeGrammar()


    val testPattern = Term("c", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty), Term("c3", Seq.empty)))
    val testPattern2 = Term("c", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty)))
    val testPatternStar = Term("c", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty), Star()))
    val testPattern1 = Term("c", Seq(Term("c1", Seq.empty)))



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
    val Z = Constructor("Z")
    val Q = Constructor("Q")

    /*val edgeCaseTree: Set[Rule] =
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
      )*/
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
        Combinator(E, "L"),
        Apply(E, Arrow(E, E), E),
        Combinator(Arrow(E, E), "M")
      )

    def isApplicativeTG(treeGrammar: TreeGrammar): Boolean = {
      var isATG = true
      treeGrammar.map(e => e._2.map(k => if (k._2.nonEmpty && !k._2.size.equals(2)) {
        isATG = false
      }))
      isATG
    }

    describe("testGrammar3 to applicative tree grammar to testGrammar3") {
      val treeGrammarRule3: Map[Type, Set[(String, Seq[Type])]] = Map(
        Constructor("A") -> Set(
          ("c", Seq(Constructor("D"), Constructor("E"), Constructor("K"), Constructor("F"), Constructor("L"), Constructor("M"), Constructor("N"))),
          ("c", Seq(Constructor("D"))),
          ("c", Seq(Constructor("D"), Constructor("F"))),
          ("f", Seq(Constructor("D"), Constructor("E"))))
      )
    }
    describe("testGrammar to applicative tree grammar to testGrammar") {

      val treeGrammarRule: Map[Type, Set[(String, Seq[Type])]] = Map(
        Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
          ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
          ("f", Seq(Constructor("D"), Constructor("E")))),
      )
      val appTree = translator.translateTGtoATG(treeGrammarRule)
      println(prettyPrintRuleSet(appTree))
      val probeTree = translatorBack.translateATGtoTG(appTree)
    }

    describe("test translate rules to tree grammar") {
      val testRulesGrammar: Set[Rule] = {
        Set[Rule](
          //Combinator(A, "F"),
          Apply(A, B, C),
          Apply(A, C, D),
          Apply(B, D, Z),
          Apply(C, D, E),
          Combinator(D, "d"),
          Combinator(E, "e"),
          Combinator(A, "e"),
          Combinator(Q, "q")
        )
      }
      val testRulesReachable: Set[Rule] = {
        Set[Rule](
          Apply(A, Arrow(B,A), B),
          Combinator(Arrow(A, B), "up"),
          Combinator(Arrow(B, A), "down"),
          Apply(B, Arrow(A, B), A),
          Combinator(B, "start"),
          Combinator(A, "left")
        )
      }

      val testRulesReachable2: Set[Rule] = {
        Set[Rule](
          Apply(Constructor("A->A"), Constructor("pA->A->A"),A),
          Apply(Constructor("A->A"), Constructor("A->A->A"),A),
          Combinator(Constructor("pA->A->A"), "up"),
          Combinator(Constructor("A->A->A"), "down"),
          Apply(A, Constructor("A->A"), A),
        )
      }

      val testRulesReachable3: Set[Rule] = {
        Set[Rule](
          Apply(Constructor("p!A->A"), Constructor("p!A->A->A"),Constructor("p,p22a!A")),
          Apply(Constructor("p!A->A"), Constructor("p,p11a!A->A->A"),Constructor("p!A")),
          Apply(Constructor("p,p1!A->A"), Constructor("p!A->A->A"),Constructor("p,p22!A")),
          Apply(Constructor("p,p1!A->A"), Constructor("p,p11!A->A->A"),Constructor("p!A")),
          Combinator(Constructor("p!A->A->A"), "up"),
          Combinator(Constructor("p,p11!A->A->A"), "up"),
          Combinator(Constructor("p,p11a!A->A->A"), "up"),
          //Combinator(Constructor("A->A->A"), "down"),
          Apply(A, Constructor("p!A->A"), Constructor("p,p2!A")),
          Apply(A, Constructor("p,p1!A->A"), Constructor("p!A")),
        )
      }
      val testTranslatorGrammar:Map[Type, Set[(String, Seq[Type])]] = Map(
        A -> Set(("down", Seq(A, A))),
        //B->Set(("start", Seq.empty), ("up", Seq(A)))
      )
      val tra = translator.translateTGtoATG(testTranslatorGrammar)
      val tgt = Constructor("A")
      val reach = filter.reachableRules(testRulesReachable2, tgt, Set.empty)._2
      val traGrammar = translatorBack.translateATGtoTG(testRulesReachable2)
        it("should be equal") {
           assert(traGrammar.exists(e=>e._1 == A && e._2.contains("up", Seq(A,A))))
         }
      /*    it("should not be equal because of Z-rule") {
           assert(!filter.reachableRules((testRulesGrammar+Combinator(Z, "z")),
             tgt = Constructor("A"), checkedTypes = Set.empty)._2.size.equals((testRulesGrammar+Combinator(Z, "z")).size))
         }
         it("should not be equal") {
           assert(!filter.reachableRules(testRulesGrammar,
             Constructor("B"), Set.empty)._2.size.equals(testRulesGrammar.size))
         }*/
    }
    describe("test forbid StarPattern in Patterns Tree Grammar") {
      val patStar= ApplyPattern(CombinatorPattern("J"), StarPattern())
      val appGrammar = translator.translateTGtoATG(treeGrammar)
      val filteredGrammar = filter.forbidApply(applicativeTreeGrammarTest, patStar)
      //println(prettyPrintRuleSet(filteredGrammar))
      it("should contain {@(J, *)}! A -> B --> J;") {
        assert(filteredGrammar.contains(Combinator(Constructor("{@(J, *)}! A -> B"), "J")))
      }
      it("should not contain {@(J, *); J}! A -> B --> J;") {
        assert(!filteredGrammar.contains(Combinator(Constructor("{@(J, *); J}! A -> B"), "J")))
      }
    }
    describe("test forbid StarPattern in Applicative Tree Grammar") {
      val patStar= StarPattern()
      val filteredGrammar = filter.forbidApply(applicativeTreeGrammarTest, patStar)
      //println(prettyPrintRuleSet(applicativeTreeGrammarTest))
      it("should be empty") {
        assert(filteredGrammar==Set())
      }
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
        .mkString("{", "; \n ", "}")
    }
    def prettyPrintTreeGrammar(grammar: TreeGrammar): String = {
      grammar
        .groupBy(_._1)
        .map {
          case (target, entries) =>
            s"${entries.mkString(" | ")}"
        }
        .mkString("{", "; \n ", "}")
    }

    lazy val tree = GammaAssociation.inhabit(Constructor("sigma3"))

    lazy val testChannel = new DebugMsgChannel()

    lazy val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)

}
