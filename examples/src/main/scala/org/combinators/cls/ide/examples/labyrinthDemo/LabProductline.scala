package org.combinators.cls.ide.examples.labyrinthDemo

import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.{Constructor, FiniteSubstitutionSpace, Type}
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}

class LabProductline @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  lazy val repository = new Repository
  override val controllerAddress = "LabProduct"
  override val projectName = controllerAddress
  override val tgts: Seq[Type] = Seq(target)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  lazy val target: Type = posToType(0, 0)
  def intToType(x: Int): Type = {
    Constructor(x.toString)
  }
  def posToType(pos: (Int, Int)): Type = {
    'Pos (intToType(pos._1), intToType(pos._2))
  }

  lazy val Gamma = ReflectedRepository(repository,
    substitutionSpace = FiniteSubstitutionSpace.empty,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())


}
