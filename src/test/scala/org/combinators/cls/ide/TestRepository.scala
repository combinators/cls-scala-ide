package org.combinators.cls.ide

import java.nio.file.{Path, Paths}

import org.combinators.cls.git.{EmptyResults, Results}
import org.combinators.cls.ide.inhabitation.{BoundedCombinatoryLogicDebugger, FiniteCombinatoryLogicDebugger, TestChannel}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import Helpers._

class TestRepository {
  val garbageCombinators =
    Map(
      "f" ->
        Arrow(
          Constructor("Int"),
          Constructor("Goal")
        ),
      "x" -> Constructor("Int"),
      "garbage1" ->
        Arrow(
          Constructor("Garbage1"),
          Intersection(Constructor("Int"), Constructor("Garbage2"))
        ),
      "garbage2" ->
        Arrow(
          Constructor("Garbage2"),
          Constructor("Garbage1")
        )
    )
  val taxonomy =
    Taxonomy("Int")
      .merge(Taxonomy("Garbage1"))
      .merge(Taxonomy("Garbage2"))
      .merge(Taxonomy("Goal"))

  lazy val testChannel = new TestChannel()
  val Gamma = Expected.expectedPaths.foldLeft(ReflectedRepository(garbageCombinators, substitutionSpace = FiniteSubstitutionSpace.empty, classLoader = this.getClass.getClassLoader)) {
    (repo, path) => repo.addCombinator(new TestCombinator(path))
  }
  val GammaFin = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(Map.empty), garbageCombinators)
  val target: Constructor = Constructor("Goal")
  lazy val results: InhabitationResult[Unit] = InhabitationResult[Unit](GammaFin.inhabit(target), target, x => ())

  lazy val jobs = Gamma.InhabitationBatchJob[Unit]('Goal)
  lazy val resultsInhabit: Results = EmptyResults().addAll(jobs.run())
  lazy val inhabitantsNumber = (resultsInhabit.raw.values.flatMap(_._2))

  lazy val jobs1 = Gamma.InhabitationBatchJob[Unit]('Int)
  lazy val resultsInhabit1: Results = EmptyResults().addAll(jobs.run())

}

object Expected {
  val expectedPaths: Set[Path] = Set(
    Paths.get("test.txt"),
    Paths.get("test", "test.txt")
  )
}

class TestCombinator(path: Path) {
  def apply: Path = path
}
