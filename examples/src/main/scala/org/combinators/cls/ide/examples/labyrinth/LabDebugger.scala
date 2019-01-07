

package org.combinators.cls.ide.examples.labyrinth

import java.nio.file.{Files, Paths}

import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents, InjectedController}
import controllers.Assets
import org.combinators.cls.git.{EmptyResults, Results}
import org.combinators.cls.ide.examples.labyrinth.Examples.lab3
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.templating.persistable.JavaPersistable._
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
  val reflectedRepository2 = ReflectedRepository(repository, substitutionSpace = FiniteSubstitutionSpace.empty, classLoader = this.getClass.getClassLoader)
  println(">>>>",reflectedRepository2.combinators)
  debugger.computeResults(Gamma, Seq(target), repository)
  println("<<<", Gamma.combinators)

}

