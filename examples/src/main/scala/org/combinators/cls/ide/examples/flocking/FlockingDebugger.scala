package org.combinators.cls.ide.examples.flocking

import javax.inject.Inject
import controllers.Assets
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.{Intersection, Type}
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class FlockingDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled
{

  override val controllerAddress = "flocking"
  override val projectName = controllerAddress
 val target = repository.leaderControllerFile
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)

  override val result = Some(Gamma.inhabit[String](repository.leaderControllerFile))

  lazy val repository = new FlockingRepository {}

  override val tgts: Seq[Type] = Seq(target)
  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = repository.kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
}

