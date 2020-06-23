
package org.combinators.cls.ide.examples.guidemo

import javax.inject.Inject
import controllers.Assets
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Omega, SubtypeEnvironment, Type}
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import HelpersGui.{Form, _}

class GuiDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  lazy val repository = new Repository
  lazy val realRepo = repository
  lazy val target: Type = 'OrderMenu (Omega)

  override val controllerAddress = "gui"
  override val projectName = controllerAddress
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  //Gamma.InhabitationBatchJob[Form]('OrderMenu(Omega))
  //Gamma.InhabitationBatchJob[Form]('OrderMenu)

  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = repository.kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
  override val result = Some(Gamma.inhabit[Form](target))

  override val tgts: Seq[Type] = Seq(result.get.target)
}