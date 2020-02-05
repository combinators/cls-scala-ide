package org.combinators.cls.ide.filter

import akka.actor.FSM.->
import com.sun.javafx.css.Combinator
import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.inhabitation.{BoundedCombinatoryLogic, FiniteCombinatoryLogic, Tree, TreeGrammar}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Arrow, Constructor, Intersection, Kinding, SubtypeEnvironment, Type, Variable}
import org.combinators.cls.types.syntax._
import org.scalatest.FunSpec

import scala.collection.mutable
import scala.collection.parallel.immutable

class FilterTest extends FunSpec {

  val filter = new FilterList()

  lazy val alpha = Variable("alpha")

  //val parser = PatternParser
  lazy val kinding: Kinding =
    Kinding(alpha).addOption('sigma1).addOption('sigma2)
  val repository =
    Map(
      "f" -> Arrow(Constructor("sigma1"), Arrow(Constructor("sigma2"), Constructor("sigma0"))),
      "g" -> Arrow(Constructor("sigma3"), Constructor("sigma0")),
      "c2" -> Constructor("sigma1"),
      "c3" -> Arrow(Constructor("sigma3"), Constructor("sigma2")),
      "c4" -> Constructor("sigma3"),
      "c5" -> Arrow(Constructor("sigma0"), Constructor("sigma3"))
    )

  val tgtSymbol: Type = Constructor("sigma0")
  val tgtSymbolFilter: Type = Constructor("p! sigma0")
  val Gamma = new FiniteCombinatoryLogic(SubtypeEnvironment(Map.empty), repository)
  lazy val testChannel = new DebugMsgChannel()
  val GammaFCL = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)

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


  var allPartGrammars: mutable.Set[TreeGrammar] = mutable.Set.empty

  var partTreeGrammar: Set[(String, Seq[String])]= Set.empty

  val muster2: Muster = Term("f", Seq(Term("c3", Seq.empty), Star()))
  val muster3: Muster = Term("id", Seq(Term("id", Seq(Term("id", Seq(Star()))))))
  val muster4: Muster = Term("f", Seq(Term("c2", Seq.empty), Term("c4", Seq.empty)))
  lazy val musterTestTerm: Muster = Term("f", Seq(Term("c4", Seq.empty)))
  val repositoryAssociation =
    Map(
      "f" -> Arrow(Constructor("X"), Arrow(Constructor("Y"), Constructor("X"))),
      "x" -> Constructor("X"),
      "y" -> Intersection(Constructor("X"), Constructor("Y")),
      "z" -> Intersection(Constructor("X"), Constructor("Y"))

    )
  val GammaAssociation = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repositoryAssociation)
  val tgtAssociation = Arrow(Constructor("Y"), Constructor("X"))
  val tgtPat = Constructor("p! Y -> X")
  val resTree = GammaAssociation.inhabit(tgtAssociation)
  resTree.foreach { case (n, rhss) =>
    println(s"$n |-> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
  }
  val pat = Term("f", Seq(Term("f", Seq(Term("x", Seq.empty), Term("y", Seq.empty))), Term("z", Seq.empty)))

  val newTree = filter.forbid(resTree, pat)

  val prune = GammaAssociation.prune(newTree, Set(tgtPat))
  val resultTerm = InhabitationResult[Unit](prune, tgtPat, x => ())
  println(".....")
  newTree.foreach { case (n, rhss) =>
    println(s"$n : ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
  }
  println(".....")
  /*
  println(".....")
  println(resultTerm.isInfinite)
  for (i <- 0 to 100) println(resultTerm.terms.index(i))
*/



  describe(s"Filter by muster with two arguments") {
    val muster: Muster = Term("f", Seq(Term("h", Seq(Star(), Star())), Star()))
    val grammar: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma1") -> Set(
          ("c2", Seq.empty[Type]),
          ("h", Seq[Type](Constructor("sigma2"), Constructor("sigma3")))
        ),
        Constructor("sigma2") -> Set(("c2", Seq.empty[Type]),
          ("c3", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("f", Seq[Type](Constructor("sigma2")))
          )
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammar, muster)
    newTreeGrammar.foreach { case (n, rhss) =>
      println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
    }

    val prune = GammaFCL.prune(newTreeGrammar, Set(tgtSymbolFilter))
    println(":::")
    prune.foreach { case (n, rhss) =>
      println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
    }
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
    it(s"the new tree grammar should be without $muster"){
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
    it("should make new treeGrammar with new sigma2") {
      assert(newTreeGrammar.exists(e => e._1.toString.startsWith("p!")))
    }
  }
  describe("Filter two star arguments") {
    val grammar: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3"))),
          ("f", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c3", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma4") -> Set(("c6", Seq.empty[Type])),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c5", Seq[Type](Constructor("sigma0")))
          )
      )
    lazy val muster: Muster = Term("f", Seq(Star(), Star()))
    lazy val musterStar: Muster = Term("f", Seq(Star()))
    describe(s"Filter by muster $muster with two arguments") {
      val newTreeGrammar: TreeGrammar = filter.forbid(grammar, muster)
      val prune = GammaFCL.prune(newTreeGrammar, Set(tgtSymbolFilter))
      val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
      it("the new tree grammar should be without muster"){
        if (results.isInfinite){
          for (index <- 0 until 10){
            assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
          }
        }else {
          for (index <- 0 to results.size.get.toInt){
            assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
          }
        }
      }
      it("should make new treeGrammar with new sigma2") {
        assert(newTreeGrammar.contains(Constructor("p,Star()! sigma2")))
      }
      it("should make new treeGrammar with new sigma1") {
        assert(newTreeGrammar.contains(Constructor("p,Star()! sigma1")))
      }
      it("should make new treeGrammar without sigma0") {
        assert(!newTreeGrammar.contains(Constructor("sigma0")))
      }
      it("should make new treeGrammar with new p,Star()! sigma3 -> Set()") {
        assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Star()! sigma3"), Set.empty)))
      }
      it("should make new treeGrammar with new p! sigma4 -> Set(c6())") {
        assert(newTreeGrammar.exists(e => e.equals(Constructor("p! sigma4"), Set(("c6",List.empty[Type])))))
      }
      it("should make new treeGrammar with new p! sigma0 -> f(p,Star()! sigma1, p! sigma2)") {
        assert(newTreeGrammar.exists(e => (e._1.equals(Constructor("p! sigma0")) && (e._2.contains(("f",List(Constructor("p,Star()! sigma1"), Constructor("p! sigma2"))))))))//, ("f",List(Constructor("p! sigma1"), Constructor("p,Star()! sigma2")))))))
      }
      it("the length should be greater or equal the length of the original") {
        assert(newTreeGrammar.size >= grammar.size)
      }
      it("should be different then the original") {
        assert(!newTreeGrammar.equals(grammar))
      }
    }

    describe(s"Filter by muster $musterStar") {
      val newTreeGrammar: TreeGrammar = filter.forbid(grammar, musterStar)
      val prune = GammaFCL.prune(newTreeGrammar, Set(tgtSymbolFilter))
      val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
      it("the new tree grammar should be without muster"){
        if (results.isInfinite){
          for (index <- 0 until 10){
            assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
          }
        }else {
          for (index <- 0 to results.size.get.toInt){
            assert(!mkTreeMap(Seq(results.terms.index(index))).contains(Seq(musterTestTerm)))
          }
        }
      }
      it("should make new treeGrammar with new sigma3") {
        assert(!newTreeGrammar.contains(Constructor("sigma3")))
      }
      it("should make new treeGrammar with new p! sigma3") {
        assert(newTreeGrammar.contains(Constructor("p! sigma3")))
      }
      it("should make new treeGrammar with new p,Star()! sigma1") {
        assert(newTreeGrammar.contains(Constructor("p,Star()! sigma1")))
      }
      it("should make new treeGrammar with new p,Star()! sigma2") {
        assert(newTreeGrammar.contains(Constructor("p,Star()! sigma2")))
      }
      it("should make new treeGrammar with new sigma1") {
        assert(!newTreeGrammar.contains(Constructor("sigma1")))
      }
      it("should make new treeGrammar without sigma0") {
        assert(newTreeGrammar.contains(Constructor("p! sigma0")))
      }
      it("should make new treeGrammar with new sigma2 -> Set()") {
        assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Star()! sigma2"), Set.empty)))
      }
      it("should make new treeGrammar with new p,Star()! sigma0 -> f(p,Star()! sigma3)") {
        assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Star()! sigma0"), Set.empty)))
      }
      // (p! sigma0,Set((f,List(p,Star()! sigma1, sigma2)), (f,List(p! sigma1, p,Star()! sigma2)), (g,List(p! sigma3)), (f,List(p! sigma3))))
      it("should make new treeGrammar with new p! sigma0 -> f(p,Star()! sigma1, p! sigma2)") {
        assert(newTreeGrammar.exists(e => (e._1.equals(Constructor("p! sigma0"))) && (e._2.contains(("f",List(Constructor("p! sigma1"), Constructor("p! sigma2")))))))
      }
      it("the length should be greater or equal the length of the original") {
        assert(newTreeGrammar.size >= grammar.size)
      }
      it("should be different then the original") {
        assert(!newTreeGrammar.equals(grammar))
      }
    }
  }
  describe("Filter two arguments") {
    val grammar: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma4"))),
          ("g", Seq[Type](Constructor("sigma3"))),
          ("f", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma1") -> Set(("c1", Seq.empty[Type]),
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2")))),
        Constructor("sigma4") -> Set(("c6", Seq.empty[Type])),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c5", Seq[Type](Constructor("sigma0")))
          )
      )
    lazy val muster: Muster = Term("f", Seq(Term("c1", Seq.empty), Term("c2", Seq.empty)))
    describe(s"Filter by muster $muster") {
      val newTreeGrammar: TreeGrammar = filter.forbid(grammar, muster)
      val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
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
      it("should make new treeGrammar with new sigma3") {
        assert(newTreeGrammar.contains(Constructor("p,Term(c2,List())! sigma3")))
      }
      it("should make new treeGrammar with new sigma1") {
        assert(newTreeGrammar.contains(Constructor("p! sigma1")))
      }
      it("should make new treeGrammar without sigma0'") {
        assert(!newTreeGrammar.contains(Constructor("sigma0")))
      }
      it("should make new treeGrammar with new sigma1' -> Set()") {
        assert(newTreeGrammar.exists(e => !e.equals(Constructor("sigma1"), Set.empty)))
      }
      it("should make new treeGrammar with new sigma2' -> Set()") {
        assert(newTreeGrammar.exists(e => !e.equals(Constructor("sigma2'"), Set.empty)))
      }
      it("the length should be greater or equal the length of the original") {
        assert(newTreeGrammar.size >= grammar.size)
      }
      it("should be different then the original") {
        assert(!newTreeGrammar.equals(grammar))
      }
    }
  }

  describe("Filter by muster with an argument") {
    val grammar: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c3", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c5", Seq[Type](Constructor("sigma0")))
          )
      )
    val muster: Muster = Term("g", Seq(Star()))
    val newTreeGrammar: TreeGrammar = filter.forbid(grammar, muster)
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
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
    it("should make new treeGrammar with new sigma3") {
      assert(newTreeGrammar.contains(Constructor("p! sigma3")))
    }
    it("should make new treeGrammar with new p! sigma3") {
      assert(newTreeGrammar.exists(e => e._2.contains("g", Seq(Constructor("p,Star()! sigma3")))))
    }
    it("should make new treeGrammar with new sigma3 -> Set()") {
      assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Star()! sigma3"), Set.empty)))
    }
    it("should make new treeGrammar without sigma0") {
      assert(newTreeGrammar.contains(Constructor("p! sigma0")))
    }
    it("the length should be greater or equal the length of the original") {
      assert(newTreeGrammar.size >= grammar.size)
    }
    it("should be different then the original") {
      assert(!newTreeGrammar.equals(grammar))
    }
  }

  describe(s"Filter by muster $muster2") {
    val grammarMap: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma1") -> Set(("c3", Seq.empty[Type]),
          ("c4", Seq.empty[Type])),
        // Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c3", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c5", Seq[Type](Constructor("sigma0")))
          )
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammarMap, muster2)
    val prune = GammaFCL.prune(newTreeGrammar, Set(tgtSymbolFilter))
    val resultsPrune = InhabitationResult[Unit](prune, tgtSymbolFilter, x => ())
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())

    it(s"the new tree pruned grammar should be without $muster2"){
      if (resultsPrune.isInfinite){
        for (index <- 0 until 10){
          assert(!mkTreeMap(Seq(resultsPrune.terms.index(index))).contains(Seq(musterTestTerm)))
        }
      }else {
        for (index <- 0 until resultsPrune.size.get.toInt){
          assert(!mkTreeMap(Seq(resultsPrune.terms.index(index))).contains(Seq(musterTestTerm)))
        }
      }
    }

    it(s"the new tree grammar should be without $muster2"){
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
    it("should not be empty") {
      assert(newTreeGrammar.contains(Constructor("p! sigma1")))
    }
    it("should make new treeGrammar with new p,Term(c3,List()),Star()! sigma1-> Set()") {
      assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Term(c3,List()),Star()! sigma1"), Set.empty)))
    }
    it("should make new treeGrammar with new p,Term(c3,List())! sigma1-> Set()") {
      assert(newTreeGrammar.exists(e => e.equals(Constructor("p,Term(c3,List())! sigma1"), Set(("c4", Seq())))))
    }
    it("should make new treeGrammar with new p! sigma1 -> c3()|c4() ") {
      assert(newTreeGrammar.exists(e => e.equals(Constructor("p! sigma1"), Set(("c3", Seq.empty), ("c4", Seq.empty)))))
    }
  }
  describe(s"Filter by muster  $muster3") {
    val grammarMap3: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma1") -> Set(("id", Seq[Type](Constructor("sigma1"))),
          ("c4", Seq.empty[Type]))
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammarMap3, muster3)
    val prune = GammaFCL.prune(newTreeGrammar, Set(tgtSymbolFilter))
    val results = InhabitationResult[Unit](grammarMap3, tgtSymbol, x => ())
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
    it("should not be empty") {
      assert(newTreeGrammar.contains(Constructor("p! sigma1")))
    }
  }
  describe(s"Filter by muster $muster4 with two arguments") {
    val grammar4: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c4", Seq.empty[Type]),
          ("c3", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c5", Seq[Type](Constructor("sigma0")))
          )
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammar4, muster4)
    println("----")
    newTreeGrammar.foreach { case (n, rhss) =>
      println(s"$n : ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
    }
    println("xxxx")
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
    it(s"the new tree grammar should be without $muster4"){
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
    it("should make new treeGrammar with new p! sigma2") {
      assert(newTreeGrammar.exists(e => e._1.toString.startsWith("p!")))
    }
  }
  describe(s"Filter by muster with an argument") {
    val muster: Muster = Term("f", Seq(Star()))
    val grammar4: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c2", Seq.empty[Type]),
          ("c3", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("f", Seq[Type](Constructor("sigma2")))
          )
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammar4, muster)
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
    it("the tree grammar should be without muster"){
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
    it("should make new treeGrammar with new p! sigma2") {
      assert(newTreeGrammar.exists(e => e._1.toString.startsWith("p!")))
    }

    it("should make new treeGrammar with new p! sigma3-> f(p,Star()! sigma2)") {
      assert(newTreeGrammar.exists(e => (e._1.equals(Constructor("p! sigma3"))) && (e._2.contains(("f",List(Constructor("p,Star()! sigma2")))))))
    }
  }
  describe(s"Filter by muster with three arguments") {
    val muster: Muster = Term("f", Seq(Star(), Star(), Star()))
    val grammar4: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),
          ("g", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma1") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma2") -> Set(("c2", Seq.empty[Type]),
          ("c3", Seq[Type](Constructor("sigma3")))
        ),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("f", Seq[Type](Constructor("sigma2")))
          )
      )
    val newTreeGrammar: TreeGrammar = filter.forbid(grammar4, muster)
    val results = InhabitationResult[Unit](newTreeGrammar, tgtSymbolFilter, x => ())
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
    it("should make new treeGrammar with new p! sigma2'") {
      assert(newTreeGrammar.exists(e => !e._1.toString.startsWith("p!")))
    }
  }
  describe(s"Filter by muster $musterTestTerm"){
    val grammar: TreeGrammar =
      Map[Type, Set[(String, Seq[Type])]](
        Constructor("sigma0") -> Set(
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2"))),

          ("g", Seq[Type](Constructor("sigma3"))),
          ("f", Seq[Type](Constructor("sigma3")))),
        Constructor("sigma1") -> Set(("c1", Seq.empty[Type]),
          ("f", Seq[Type](Constructor("sigma1"), Constructor("sigma2")))),
        Constructor("sigma2") -> Set(("c2", Seq.empty[Type])),
        Constructor("sigma4") -> Set(("c6", Seq.empty[Type])),
        Constructor("sigma3") ->
          Set(
            ("c4", Seq.empty[Type]),
            ("c3", Seq[Type](Constructor("sigma3")))
          )
      )
    val newTreeGrammarTestTerm: TreeGrammar = filter.forbid(grammar, musterTestTerm)
    val results = InhabitationResult[Unit](newTreeGrammarTestTerm, tgtSymbolFilter, x => ())
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
  }
}







