package org.combinators.cls.ide.examples.scaleSize
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.{Constructor, FiniteSubstitutionSpace, Type}
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}

class ScaleProductline @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {
  lazy val scale = new ScaleLabyrinth()
  lazy val repository = scale.movements ++ scale.freeFields
  override val controllerAddress = "scale"
  override val projectName = controllerAddress
  override val tgts: Seq[Type] = Seq(scale.tgt)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val result = Some(Gamma.inhabit[Unit](scale.tgt))
 // override val reposit: Option[Map[String, Type]] = Some(repository)
  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = scale.kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
}

