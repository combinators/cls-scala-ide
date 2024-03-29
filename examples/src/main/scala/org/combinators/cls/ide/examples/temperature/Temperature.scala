
package org.combinators.cls.ide.examples.temperature

import com.github.javaparser.ast.expr.Expression
import javax.inject.Inject
import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle


class Temperature @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {
  // Note: This will produce two variations; only the first is deemed accurate, and it is interesting
  // to consider how to deny the synthesis of the second one...
  val repository = new Repository {}

  import repository._

  override val controllerAddress = "temperature"
  override val projectName = controllerAddress
  override val tgts: Seq[Type] = Seq(target)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  lazy val target: Type = (artifact('Impl)) :&: precision(precision.floating)

  override val result = Some(Gamma.inhabit[Expression](target))
  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = repository.precisions.merge(units),
    semanticTaxonomy = taxonomyLoss,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())

}
    //alternative goals:
    /* lazy val target: Type = (artifact(artifact.impl) :&: precision(precision.integer) :&: unit(unit.celsius))

  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = repository.precisions.merge(units),
    semanticTaxonomy = taxonomyLoss,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())

  debugger.computeResults(Gamma, Seq(target))

  // Omega is like Object -- the base type everything inherits from
  //Gamma.inhabit[JType](precision(Omega))
}*/


