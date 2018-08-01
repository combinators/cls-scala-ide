package org.combinators.cls.ide

import java.nio.file.{Path, Paths}

import org.combinators.cls.git.{EmptyResults, Results}
import org.combinators.cls.ide.inhabitation.TestChannel
import org.combinators.cls.interpreter.ReflectedRepository
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
  val Gamma = Expected.expectedPaths.foldLeft(ReflectedRepository(garbageCombinators, substitutionSpace = FiniteSubstitutionSpace.empty, classLoader = this.getClass.getClassLoader)){
    (repo, path) => repo.addCombinator(new TestCombinator(path))
  }
  val target: Constructor = Constructor("impossible")

  lazy val jobs = Gamma.InhabitationBatchJob[Unit]('Int)
  lazy val resultsIntabit: Results = EmptyResults().addAll(jobs.run())

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
