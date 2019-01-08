

package org.combinators.cls.ide.examples.labyrinth

import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class LabDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  val labExample = Examples.lab3_1
  lazy val repository = labExample.moves
  override val controllerAddress = "labyrinth"
  override val projectName = controllerAddress
  lazy val target: Type = labExample.position(labExample.goal)
  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = FiniteSubstitutionSpace.empty,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
  val newGamma = Gamma
  debugger.computeResults(Gamma, Seq(target), Some(repository))


}

