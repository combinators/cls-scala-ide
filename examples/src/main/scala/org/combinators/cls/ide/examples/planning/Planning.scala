package org.combinators.cls.ide.examples.planning
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.types.syntax._
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.types.Type

class Planning @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  lazy val repository = new Repository
  lazy val target: Type = 'PrintConstructionDrawings

  override val controllerAddress = "planning"
  override val projectName = controllerAddress
  override val tgts: Seq[Type] = Seq(target)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)

  override val result = Some(Gamma.inhabit[String](target))
  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = repository.kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())

}
