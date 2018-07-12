
package org.combinators.cls.ide.examples.guidemo

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.ide.{Debugger, RoutingEntries}
import org.combinators.cls.types.SubtypeEnvironment

class GuiDemoDebugger @Inject()(productline: Productline)
  extends Debugger(
    productline.webJars,
    productline.assets,
    productline.Gamma.substitutionSpace,
    SubtypeEnvironment(productline.Gamma.nativeTypeTaxonomy.addNativeType[CompilationUnit].taxonomy.merge(productline.Gamma.semanticTaxonomy).underlyingMap),
    productline.jobs.targets,
    productline.results.infinite,
    productline.combinatorComponents,
    productline.results,
    productline.testChannel,
    productline.projectName) with RoutingEntries {
  override val controllerAddress: String = "gui"
}

