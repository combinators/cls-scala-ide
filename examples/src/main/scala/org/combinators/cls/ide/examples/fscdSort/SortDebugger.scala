package org.combinators.cls.ide.examples.fscdSort

import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class SortDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  override val controllerAddress = "sort"
  override val projectName = controllerAddress
  lazy val alpha = Variable("alpha")
  lazy val kinding : Kinding = Kinding(alpha).addOption(Constructor("Double"))
    .addOption(Constructor("List", Constructor("Double"))).addOption('sorted).addOption('minimal)

  val sortRepo = Map(
    "values" -> Constructor("List", Constructor("Double")),
    "id" -> Arrow(alpha, alpha),
    "inv"  -> Arrow(Constructor("Double"), Constructor("Double")),
    "sort" -> Arrow(Arrow(alpha,Constructor("Double")),
              Arrow(Constructor("List", alpha),Intersection(Constructor("List", alpha), 'sorted))),
    "hd" -> Arrow(Constructor("Double"),
            Arrow(Intersection(Constructor("List", Constructor("Double")), 'sorted),
              Intersection(Constructor("Double"), 'minimal))),
    "default" -> Constructor("Double")
  )

  lazy val target: Type = 'Double :&: 'minimal
  lazy val Gamma = ReflectedRepository(sortRepo,
    substitutionSpace = kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
  lazy val resultSort = debugger.computeResults(Gamma, Seq(target), Some(sortRepo))
  println("RESULT", resultSort)

}
