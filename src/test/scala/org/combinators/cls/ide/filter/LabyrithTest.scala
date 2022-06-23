package org.combinators.cls.ide.filter

import org.combinators.cls.ide.inhabitation.{DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.inhabitation.{FiniteCombinatoryLogic, Tree, TreeGrammar}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types.{Constructor, SubtypeEnvironment, Type}
import org.scalatest.FunSpec
import scala.util.control.Breaks._


class LabyrinthTest extends FunSpec {
  val example = Examples.lab2
  val repository = example.moves
  val tgts = example.position(example.goal)
  val filter = new FilterList()

  val testChannel = new DebugMsgChannel()
  val Gamma = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)
  val result = Gamma.inhabit(tgts)

  val strType: String = s"p! ${tgts.toString()}"
  val tgtSymbolFilter: Type = Constructor(strType)

  def mkTreeMap(trees: Seq[Tree]): Seq[Muster] = {
    var seqTree: Seq[Muster] = Seq.empty
    for (tree <- trees) {
      seqTree = Seq(tree match {
        case Tree(name, _, arguments@_*) =>
          Term(name, mkTreeMap(arguments))
        case _ => Star()
      })
    }
    seqTree
  }
  describe("Filter of terms") {

    val muster: Muster = Term("down", Seq(Term("down", Seq(Term("up", Seq(Star()))))))
    val newTree = filter.forbid(result, muster)
    val prune = Gamma.prune(newTree, Set(tgts))
    val results = InhabitationResult[Unit](prune, tgtSymbolFilter, x => ())
    val term: Muster = Term("up", Seq(Term("right", Seq(Term("up", Seq(Term("start", Seq.empty)))))))
    it(s"the new tree grammar should contain $term") {
      if (results.isInfinite) {
        breakable {
          for (index <- 0 to 1000) {
            val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            if (currentTerm.equals(Seq(term))) {
              assert(currentTerm.equals(Seq(term)))
              break
            }else{
              assert(!currentTerm.equals(Seq(term)))
            }
          }
        }
      } else {
        breakable {
          for (index <- 0 until results.size.get.toInt) {
            val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            if (currentTerm.equals(Seq(term))) {
              assert(currentTerm.equals(Seq(term)))
              break
            }
          }
        }
      }
    }
  }
  describe("Test of filter of terms"){
    val termExists: Muster = Term("up", Seq(Term("left", Seq(Term("right", Seq(Term("left",
      Seq(Term("up", Seq(Term("up", Seq(Term("right", Seq(Term("right", Seq(Term("down", Seq(Term("start", Seq.empty)))))))))))))))))))
    val termNotExists: Muster = Term("up", Seq(Term("left", Seq(Term("right", Seq(Term("left",
      Seq(Term("up", Seq(Term("up", Seq(Term("right", Seq(Term("right", Seq(Term("down", Seq(Term("start", Seq.empty)))))))))))))))))))
    val muster: Muster = Term("up", Seq(Term("right", Seq(Star()))))
    //val muster: Muster = Term("down", Seq(Term("down", Seq(Term("up", Seq(Star()))))))
    val newTree = filter.forbid(result, muster)
    val prune = Gamma.prune(newTree, Set(tgts))
    val results = InhabitationResult[Unit](prune, tgtSymbolFilter, x => ())
    it(s"the new tree grammar should not contain $termExists") {
      if (results.isInfinite) {
        breakable {
          for (index <- 0 to 1000) {
            val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            if (currentTerm.equals(Seq(termExists))) {
              assert(currentTerm.equals(Seq(termExists)))
              break
            }else{
              assert(!currentTerm.equals(Seq(termExists)))
            }
          }
        }
      } else {
        breakable {
          for (index <- 0 until results.size.get.toInt) {
            val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            if (currentTerm.equals(Seq(termExists))) {
              assert(currentTerm.equals(Seq(termExists)))
              break
            }
          }
        }
      }
    }
    it("the new tree grammar should contain muster") {
      val term: Muster = Term("start", Seq.empty)
      if (results.isInfinite) {
        for (index <- 0 until 10) {
          val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            assert(!currentTerm.equals(Seq(termExists)))
        }
      } else {
        for (index <- 0 until results.size.get.toInt) {
          val currentTerm = mkTreeMap(Seq(results.terms.index(index)))
            assert(!currentTerm.equals(Seq(term)))

        }
      }
    }
  }
}







