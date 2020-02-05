package org.combinators.cls.ide.filter

import org.combinators.cls.ide.inhabitation.{BoundedCombinatoryLogicDebugger, DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.inhabitation.BoundedCombinatoryLogic
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.scalatest.FunSpec
import java.util.concurrent.TimeUnit

import scala.util.Random

class LabTest extends FunSpec {

var noTerm: Boolean = true
while(noTerm) {
  try{
    val results = mkLab
    results.terms.index(0)
    noTerm = false
  }catch{
    case e: IndexOutOfBoundsException =>
      noTerm = true
      println("There is no term", e)
      //mkLab
  }
}

  def mkLab = {
    val filter = new FilterList
    val filterOld = new Filter
    val labyrinthSize = 5
    // val start: (Int, Int) = (Random.nextInt(labyrinthSize), Random.nextInt(labyrinthSize))
    val start: (Int, Int) = (1,1)
    //val goal: (Int, Int) = (Random.nextInt(labyrinthSize), Random.nextInt(labyrinthSize))
    val goal: (Int, Int) = (labyrinthSize-1,labyrinthSize-1)

    val positionRow = Variable("posRow")
    val positionColumn = Variable("posCol")
    lazy val testChannel = new DebugMsgChannel()
    def intToType(x: Int): Type =
      (1 to x).foldLeft[Type]('Z)((n, _) => 'S (n))

    def anyPos(v: Variable): Kinding =
      (1 to labyrinthSize).foldLeft(Kinding(v))((k, n) => k.addOption(intToType(n)))

    val kinding = anyPos(positionRow).merge(anyPos(positionColumn))
    val tgt = 'Pos (intToType(goal._1), intToType(goal._2))
    val filterTgt = Constructor("p! Pos(S(S(Z)) * S(S(Z)))")
    val blocked: Array[Array[Boolean]] = {
      val arr = Array.ofDim[Boolean](labyrinthSize, labyrinthSize).map(row => row.map(_ => false))
      //val arr = Array.ofDim[Boolean](labyrinthSize, labyrinthSize).map(row => row.map(_ => Random.nextBoolean()))
      arr(start._2).update(start._1, false)
      arr
    }
    val freeFields: Map[String, Type] =
      blocked.indices.foldLeft(Map.empty[String, Type]) {
        case (m, row) =>
          blocked(row).indices.foldLeft(m) {
            case (m, col) if !blocked(row)(col) =>
              m.updated(s"Pos_at_($row, $col)", 'Free (intToType(row), intToType(col)))
            case _ => m
          }
      }
    val movements: Map[String, Type] =
      Map(
        "start" -> 'Pos (intToType(start._1), intToType(start._2)),
        "up" -> ('Pos (positionRow, 'S (positionColumn)) =>: 'Free (positionRow, positionColumn) =>: 'Pos (positionRow, positionColumn)),
        "down" -> ('Pos (positionRow, positionColumn) =>: 'Free (positionRow, 'S (positionColumn)) =>: 'Pos (positionRow, 'S (positionColumn))),
        "left" -> ('Pos ('S (positionRow), positionColumn) =>: 'Free (positionRow, positionColumn) =>: 'Pos (positionRow, positionColumn)),
        "right" -> ('Pos (positionRow, positionColumn) =>: 'Free ('S (positionRow), positionColumn) =>: 'Pos ('S (positionRow), positionColumn))
      )
    lazy val Gamma =  new BoundedCombinatoryLogicDebugger(testChannel, kinding, SubtypeEnvironment(Map.empty), repository)
    lazy val repository = movements++freeFields
    println("tgt", start, (intToType(start._1), intToType(start._2)))
    println("goal", goal, tgt)
    val resultedTree = Gamma.inhabit(tgt)
    val results = InhabitationResult[Unit](resultedTree, tgt, x => ())


    val labyrinth = s"\n ${blocked.map(row => row.map(e => if (e) "x" else " ").mkString("|", "|", "|")).mkString("\n")}"
    println(labyrinth)
    //lazy val Gamma = new BoundedCombinatoryLogic(kinding, SubtypeEnvironment(Map.empty), movements ++ freeFields)

    val Gamma2 = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), repository)
    if(resultedTree.nonEmpty) {
      val muster: Muster = Term("down", Seq(Term("up", Seq(Star(), Star())), Star()))
      //val muster: Muster = Term("down", Seq(Term("down", Seq(Term("up", Seq(Star()))))))


      val t1 = System.nanoTime()
      val newTreeNeu = filter.forbid(resultedTree, muster)
      val duration1 = (System.nanoTime() - t1)

      val t2 = System.nanoTime()
      val newTreeOld = filterOld.forbid(resultedTree, muster)
      val duration2 = (System.nanoTime() - t2)
      println("pattern", muster)
      println("repository size", repository.size)
      println("treeGrammar size", resultedTree.size)
      println("time New", duration1.asInstanceOf[Double] / 1000000000.0)
      println("size New", newTreeNeu.size)
      println("size Prune New", pruneNew.size)
      println("time Old", duration2.asInstanceOf[Double] / 1000000000.0)
      println("size Old", newTreeOld.size)
      println("size Prune Old", pruneOld.size)
      lazy val pruneNew = Gamma2.prune(newTreeNeu, Set(filterTgt))
      lazy val pruneOld = Gamma2.prune(newTreeOld, Set(tgt))

      /* println(">>>>>>>")
      resultedTree.foreach { case (n, rhss) =>
        println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
      }
      println("....")
      newTreeNeu.foreach { case (n, rhss) =>
        println(s"$n -> ${rhss.map { case (c, args) => s"$c${args.mkString("(", ",", ")")}" }.mkString("|") }")
      }
      println(results.terms.index(0))
    }*/
    }
    results
  }


}
