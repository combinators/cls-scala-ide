package org.combinators.cls.ide.examples.temperature

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.ide.{Debugger, RoutingEntries}
import org.combinators.cls.types.SubtypeEnvironment

class TemperatureDebugger @Inject()(temperature: Temperature)
  extends Debugger (
    temperature.webJars,
    temperature.assets,
    temperature.Gamma.substitutionSpace,
    SubtypeEnvironment(temperature.Gamma.nativeTypeTaxonomy.addNativeType[CompilationUnit].taxonomy.merge(temperature.Gamma.semanticTaxonomy).underlyingMap),
    temperature.jobs.targets,
    temperature.results.infinite,
    temperature.combinatorComponents,
    temperature.results,
    temperature.testChannel,
    temperature.projectName) with RoutingEntries {
  override val controllerAddress: String = "temperature"
}

