package org.combinators.cls.ide.filter

import org.combinators.cls.ide.filter.FilterApply
import org.combinators.cls.ide.inhabitation.{BoundedCombinatoryLogicDebugger, DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.ide.translator
import org.combinators.cls.ide.translator.{ApplicativeTreeGrammarToTreeGrammar, Apply, Combinator, Failed, Rule, TreeGrammarToApplicativeTreeGrammar}
import org.combinators.cls.inhabitation
import org.combinators.cls.inhabitation.{Tree, TreeGrammar}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types._
import org.scalatest.FunSpec

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

class FilterApplyTest extends FunSpec {

  val filter = new FilterApply()
  val translator = new TreeGrammarToApplicativeTreeGrammar()
  val translatorBack = new ApplicativeTreeGrammarToTreeGrammar()


  val testPattern = Term("c", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty), Term("c3", Seq.empty)))
  val testPatternAssoc = Term("c", Seq(Star(), Term("c", Seq(Star(), Star()))))
  val testPatternAssocDouble = Term("c", Seq(Term("c", Seq(Term("e", Seq.empty), Term("x", Seq()))), Term("d", Seq.empty)))
  //val testPatternAssocDouble = Term("c", Seq(Term("c", Seq(Term("e", Seq.empty), Term("x", Seq(Star())))), Term("d", Seq.empty)))
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
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),("c", Seq(Constructor("F"), Constructor("D"))), ("f", Seq(Constructor("D"), Constructor("E")))),
      Constructor("B") -> Set(("c", Seq(Constructor("D"), Constructor("E")))),
      Constructor("D") -> Set(("d", Seq(Constructor("B"))), ("d", Seq()), ("c", Seq(Constructor("E"),Constructor("F")))),
      Constructor("E") -> Set(("e", Seq()),("k", Seq())),
      //Constructor("F") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq(Constructor("A")))),
      Constructor("F") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()), ("x", Seq.empty)),
      Constructor("Z") -> Set(("y", Seq(Constructor("B"))), ("d", Seq()))
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
     // ("c", Seq(Constructor("A"), Constructor("B"))),
      ("c1", Seq.empty)),
    Constructor("B") -> Set(
      ("c2", Seq.empty))
  )

  describe("testAssociation vvvvv") {
  val results = InhabitationResult[Unit](treeGrammar, Constructor("S"), x => ())

    println("....", results.size)
  for(i <- 0 until  results.size.get.toInt){
    println("....", results.grammar)
    println(results.terms.index(i))

    println("....")
  }}

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

  describe("testAssociation ") {
    val treeGrammar: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(
        ("GC", Seq(Constructor("A"), Constructor("A"))),
        ("MC", Seq(Constructor("Cr"), Constructor("Cf"))),
        ("SR", Seq(Constructor("C")))),
      Constructor("C") -> Set(
        ("SR", Seq.empty),
        ("SF", Seq.empty)),
      Constructor("Cr") -> Set(
        ("SR", Seq.empty)),
      Constructor("Cf") -> Set(
        ("SF", Seq.empty)),
      Constructor("B") -> Set(
        ("AS", Seq(Constructor("A"))))
    )
    val patternAssoc = Term("GC", Seq(Star(), Term("GC", Seq(Star(), Star()))))
    val patternAssocDouble = Term("GC", Seq(Term("GC", Seq(Term("SR", Seq(Star())), Term("SR", Seq()))), Term("SR", Seq(Star()))))
    val patternUse = Term("GC", Seq(Term("GC", Seq(Star(), Term("SR", Seq(Star())))), Term("SR", Seq(Star()))))
    val tgt = "{@(@(GC, *), @(@(GC, *), *))}! B"
    val tgtUse = "{@(@(GC, @(@(GC, *), @(SR, *))), @(SR, *))}! B"

    val translatedApp = translator.translateTGtoATG(treeGrammar)
    //println(prettyPrintRuleSet(translatedApp))
    val filteredFirst = filter.forbidApply(translatedApp, filter.translatePatternToApply(patternUse))
    val filteredFirst4 = filter.forbidApply(filteredFirst, filter.translatePatternToApply(patternAssocDouble))
    val filteredFirst5 = filter.forbidApply(filteredFirst, filter.translatePatternToApply(patternUse))
    val filteredFirst2 = filter.reachableRules(filter.prune(filteredFirst),Constructor(tgtUse), Set.empty)._2
    val filteredFirst3 = filter.prune(filteredFirst2)

    println("---------")
    //println(prettyPrintRuleSet(filteredFirst5))
    println("XXXXXXXXX")
    val pw =new BufferedWriter(new FileWriter(new File("AppGraFiltered.txt")))
    val terms = prettyPrintRuleSet(filteredFirst2)
    pw.write(terms.toString())
    pw.write("\n")
    pw.close()
    val pw3 =new BufferedWriter(new FileWriter(new File("treeGrammar.txt")))
    val terms2 = prettyPrintTreeGrammar(translatorBack.translateATGtoTG(filteredFirst3))
    pw3.write(terms2)
    pw3.write("\n")
    pw3.close()
    //println(prettyPrintRuleSet(filteredFirst3))
    //println(prettyPrintTreeGrammar(filter.reachableTreeGrammar(GammaAssociation.prune(translatorBack.translateATGtoTG(filteredFirst3), Set(Constructor(tgt))), Seq(Constructor(tgt)),  Map.empty, Set.empty)._1))
    println("XXXXXXXXX")
    //println(prettyPrintTreeGrammar(translatorBack.translateATGtoTG(filteredFirst)))
    println("-----")
    //println(prettyPrintTreeGrammar(translatorBack.translateATGtoTG(filteredFirst)))
    //println(prettyPrintRuleSet(filteredFirst3))
    val newFilRes = Some(InhabitationResult[Unit](translatorBack.translateATGtoTG(filteredFirst2),
      Constructor(tgtUse), x=>()))
    val pw2 =new BufferedWriter(new FileWriter(new File("Filter.txt")))
    for (index <- 0 until 1000){

      println("result in ", newFilRes.get.size)
      val terms3 = mkTreeMap(Seq(newFilRes.get.terms.index(index)))
      pw2.write(terms3.toString())
      pw2.write("\n")

    }
    pw2.close()
  }
  describe("testGrammar to applicative tree grammar to testGrammar") {

    val treeGrammarRule: Map[Type, Set[(String, Seq[Type])]] = Map(
      Constructor("A") -> Set(("c", Seq(Constructor("D"), Constructor("E"))),
        ("c", Seq(Constructor("D"))), ("c", Seq(Constructor("D"), Constructor("F"))),
        ("f", Seq(Constructor("D"), Constructor("E")))),
    )

  }


  describe("patter c(c1, c2, c3) to applicative pattern @(@(@(c, c1), c2), c3)") {
    val applicativePattern = filter.translatePatternToApply(testPattern)
   it("should be equal to :") {
      assert(applicativePattern.toString.equals(s"@(@(@(c, c1), c2), c3)"))
    }
  }

  describe("test translate Pattern c(c1, c2) to @(@(c, c1), c2)") {
    val pattern = ApplyPattern(ApplyPattern(CombinatorPattern("c"), CombinatorPattern("c1")), CombinatorPattern("c2"))
    it("should be equal to :") {
  /*    assert(applicativePattern.toString.
        equals(s"ApplyPattern(ApplyPattern(CombinatorPattern(c),CombinatorPattern(c1)),CombinatorPattern(c2))"))
   */ }
  }
  describe("test ForbidApply") {
    val pattern = filter.translatePatternToApply(testPattern2)
    it("should be equal to :") {
    }
  }
  describe("test forbid in Tree Grammar") {
    val pat = Term("c", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty)))
    val pattern = filter.translatePatternToApply(pat)
    val appGrammar = translator.translateTGtoATG(treeGrammar)
    val filteredGrammar = filter.forbidApply(appGrammar, pattern)
    it("should contain : Apply({@(@(c, c1), c2); c2; c}! B -> S,{@(@(c, c1), c2); @(c, c1)}! A -> B -> S,{@(@(c, c1), c2)}! A)") {
      assert(filteredGrammar.contains(Apply(Constructor("{@(@(c, c1), c2); c2; c}! B -> S"), Constructor("{@(@(c, c1), c2); @(c, c1)}! A -> B -> S"),
        Constructor("{@(@(c, c1), c2)}! A"))))
    }
    it("should contain:  {@(@(c, c1), c2); c}! B --> c2 ") {
      assert(filteredGrammar.contains(Combinator(Constructor("{@(@(c, c1), c2); c}! B"), "c2")))
    }
    it("should not contain:  {@(@(c, c1), c2); c; c2}! B --> c2 ") {
      assert(!filteredGrammar.contains(Combinator(Constructor("{@(@(c, c1), c2); c; c2}! B"), "c2")))
    }
  }
  describe("test forbid StarPattern in Tree Grammar") {
    val patStar= StarPattern()
    val appGrammar = translator.translateTGtoATG(treeGrammar)
    val filteredGrammar = filter.forbidApply(appGrammar, patStar)
    it("should be empty") {
      assert(filteredGrammar==Set())
    }
  }
  describe("test reachable rules") {
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
    val tgt = Constructor("A")
    val reach = filter.reachableRules(testRulesReachable, tgt, Set.empty)._2
    val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), Map.empty)

    val resTreeAssociation = GammaAssociation.inhabit(tgt)
    val results = InhabitationResult[Unit](testGrammar3, Constructor("A"), x => ())
    //println(results.size)
    //val trPat = filter.translatePatternToApply(testPatternAssoc)
    val trPatDouble = filter.translatePatternToApply(testPatternAssocDouble)

   // for(i <- 0 until 100) println(results.terms.index(i))
    val filt = filter.forbidApply(translator.translateTGtoATG(testGrammar3),
      trPatDouble)
    val pruneAsso = filter.prune(filt)

    val newTarget = "{@(@(c, @(@(c, e), x)), d)}! A"
    val newResultsAcco = InhabitationResult[Unit](translatorBack.translateATGtoTG(pruneAsso), Constructor(newTarget), x => ())

    //println("xxxx", newResultsAcco.isInfinite)
    //println("xxxx", trPatDouble)
    val pw =new BufferedWriter(new FileWriter(new File("Assoc.txt")))
    for (index <- 0 until 1000){

      val terms = mkTreeMap(Seq(newResultsAcco.terms.index(index)))
      pw.write(terms.toString())
      pw.write("\n")

      assert(!terms.contains(Seq(testPatternAssoc)))
    }
    pw.close()

  /*  val filtDouble = filter.forbidApply(filt, filter.translatePatternToApply(testPatternAssocDouble))


    //println(prettyPrintRuleSet(filtDouble))
    val newTargetDouble = "{@(@(c, @(@(c, e), @(x, *))), d)}! {@(@(c, *), @(@(c, *), *))}! A"

    val prune = filter.prune(filtDouble)

    //new PrintWriter("AppTreeGrammar.txt") { write(prettyPrintRuleSet(prune)); close }
    //println(prettyPrintTreeGrammar(translatorBack.translateATGtoTG(filt)))
    val newResults = InhabitationResult[Unit](translatorBack.translateATGtoTG(prune), Constructor(newTargetDouble), x => ())
    println(".....", newResults.isInfinite)
    println(results.size)
   // for(i <- 0 until 100) println(newResults.terms.index(i))
    println("<<<<<")
    if (results.isInfinite){
      //val pw =  new PrintWriter(new File("Terms.txt"))
      val pw =new BufferedWriter(new FileWriter(new File("Terms.txt")))
      for (index <- 0 until 100){

        val terms = mkTreeMap(Seq(newResults.terms.index(index)))
        pw.write(terms.toString())
        pw.write("\n")

        assert(!terms.contains(Seq(testPatternAssoc)))
      }
      pw.close()
    }else {
      for (index <- 0 until results.size.get.toInt){
        assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(testPatternAssoc)))
      }
    }*/
  }
  //  val reachableGrammar = filter.reachableRules(prune, tgt, Set.empty)
 /*   it("should be equal") {
      assert(filter.reachableRules(testRulesGrammar, Constructor("A"), Set.empty)._2.size.equals(testRulesGrammar.size))
    }
    it("should not be equal because of Z-rule") {
      assert(!filter.reachableRules((testRulesGrammar+Combinator(Z, "z")),
        tgt = Constructor("A"), checkedTypes = Set.empty)._2.size.equals((testRulesGrammar+Combinator(Z, "z")).size))
    }
    it("should not be equal") {
      assert(!filter.reachableRules(testRulesGrammar,
        Constructor("B"), Set.empty)._2.size.equals(testRulesGrammar.size))
    }*/

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
  describe("test forbid Patterns in Applicative Tree Grammar") {
    val pattern= CombinatorPattern("J")
    val filteredGrammar = filter.forbidApply(applicativeTreeGrammarTest, pattern)
   // println(prettyPrintRuleSet(filteredGrammar))
    it("should not contain for example: S |-> J") {
      assert(!filteredGrammar.contains(Combinator(Constructor("{J}! A -> B"), "J")))
    }
    it("should contain {J}! C --> K") {
      assert(filteredGrammar.contains(Combinator(Constructor("{J}! C"), "K")))
    }
  }
  describe("test reachable") {
    val pattern= CombinatorPattern("J")
    val filteredGrammar = filter.forbidApply(applicativeTreeGrammarTest, pattern)
    val newTarget = Constructor("{J}! A")

 //   println("rrr", filteredGrammar.size)
    //println("rrr", prettyPrintRuleSet(filteredGrammar))
    val todoGrammar = filter.prune(filteredGrammar)

    println("rrr", filteredGrammar.size)
    println("rrr", todoGrammar.size)
    //println("rrr", prettyPrintRuleSet(todoGrammar))
    val reachableGrammar = filter.reachableRules(todoGrammar, newTarget, Set.empty)
    println("rrr", reachableGrammar._2.size)
    //println("rrr", prettyPrintRuleSet(reachableGrammar._2))
    val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), Map.empty)
    val tgtAssociation = Constructor("X")
    val tgtPat = Constructor("p! X")
    val resTreeAssociation = GammaAssociation.inhabit(tgtAssociation)
    //val newTree = filter.forbid(resTreeAssociation, pat)
    val prune = GammaAssociation.prune(translatorBack.translateATGtoTG(reachableGrammar._2), Set(newTarget))

  //  println("ttt", translatorBack.translateATGtoTG(reachableGrammar._2))
   // println("rrr", prettyPrintRuleSet(translator.translateTGtoATG(prune)))
    it("should not contain for example: S |-> J") {
      assert(!filteredGrammar.contains(Combinator(Constructor("{J}! A -> B"), "J")))
    }
    it("should contain {J}! C --> K") {
      assert(filteredGrammar.contains(Combinator(Constructor("{J}! C"), "K")))
    }
  }

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

  describe("test forbid Pattern in Terms") {
    val pattern= CombinatorPattern("J")
    val newTreeGrammarTestTerm = filter.forbidApply(applicativeTreeGrammarTest, pattern)
    val results = InhabitationResult[Unit](translatorBack.translateATGtoTG(newTreeGrammarTestTerm), Constructor("A"), x => ())
    val musterTestTerm = Term("J", Seq())
    def mkTreeMap(trees: Seq[Tree]): Seq[Muster] = {
      var seqTree: Seq[Muster] = Seq.empty
      for (tree <- trees){
        seqTree = Seq(tree match {
          case Tree(name, _, arguments@_*)=>
            Term(name, mkTreeMap(arguments))
          case _ => Star()
        })
      }
      seqTree
    }

    //println(prettyPrintRuleSet(newTreeGrammarTestTerm))
    it("the new tree grammar should be without muster"){
      if (results.isInfinite){
        for (index <- 0 until 10){
          assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
        }
      }else {
        for (index <- 0 until results.size.get.toInt){
          assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
        }
      }
    }
    it("should contain {J}! C --> K") {
      assert(newTreeGrammarTestTerm.contains(Combinator(Constructor("{J}! C"), "K")))
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