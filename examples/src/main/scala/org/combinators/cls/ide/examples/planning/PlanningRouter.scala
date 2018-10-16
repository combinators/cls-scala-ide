package org.combinators.cls.ide.examples.planning

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.ide.{Debugger, RoutingEntries}
import org.combinators.cls.types.SubtypeEnvironment


class PlanningDebugger @Inject()(planning: Planning)
  extends Debugger (
      planning.webJars,
      planning.assets,
      planning.Gamma.substitutionSpace,
      SubtypeEnvironment(planning.Gamma.nativeTypeTaxonomy.addNativeType[CompilationUnit].taxonomy.merge(planning.Gamma.semanticTaxonomy).underlyingMap),
      planning.jobs.targets,
      planning.results.infinite,
      planning.combinatorComponents,
      planning.results,
      planning.testChannel,
      planning.projectName) with RoutingEntries {
    override val controllerAddress: String = "planning"

}
