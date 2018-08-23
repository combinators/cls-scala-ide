package org.combinators.cls.ide.examples.labyrinth

import javax.inject.Inject
import controllers.Assets
import org.combinators.cls.git.EmptyResults
import org.combinators.cls.types.Kinding
import org.webjars.play.WebJarsUtil
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._
import Helpers._
import org.combinators.cls.ide.{Debugger, RoutingEntries}

/*class LabRouter @Inject()(controller: LabyrinthDebugger)
  extends SimpleRouter {
  override def routes: Routes = {
    case GET(p"/") => controller.index()
    case GET(p"/graph") => controller.showGraph()
    case GET(p"/repository") => controller.showRepo()
    case GET(p"/toggleCycle/${int(step)}") => controller.toggleCycles(step)
    case GET(p"/showDebuggerMessages") => controller.showDebuggerMessages()
    case GET(p"/showUninhabitedTy") => controller.showUninhabitedTy()
    case GET(p"/showUnusableCMsg") => controller.showUnusableCMsg()
    case GET(p"/showUnusableBecauseOfTy") => controller.showUnusableBecauseOfTy()
    case GET(p"/steps/${int(step)}") => controller.showSteps(step)
    case GET(p"/computeRequest/${label}") => controller.computeRequest(label)
    //case GET(p"/showResult/${long(index)}") => controller.showResult(index)
    case GET(p"/showPosition/${label}") => controller.showPosition(label)
  }
}*/

class LabyrinthDebugger(labyrinthName: String, labyrinth: Repository, webJars: WebJarsUtil, assets: Assets)
  extends Debugger(
    webJars,
    assets,
    Kinding.empty,
    labyrinth.Gamma.repository,
    labyrinth.Gamma.subtypes,
    Seq(labyrinth.target),
    labyrinth.results.isInfinite,
    EmptyResults().add(labyrinth.results),
    labyrinth.testChannel,
    labyrinthName) with RoutingEntries {
  override val routingPrefix: Option[String] = Some("labyrinth")
  override val controllerAddress: String = labyrinthName
}

class Labyrinth1 @Inject()(webJars: WebJarsUtil, assets: Assets)
  extends LabyrinthDebugger("lab1", Examples.lab1, webJars, assets)
class Labyrinth2 @Inject()(webJars: WebJarsUtil, assets: Assets)
  extends LabyrinthDebugger("lab2", Examples.lab2, webJars, assets)
class Labyrinth3_1 @Inject()(webJars: WebJarsUtil, assets: Assets)
  extends LabyrinthDebugger("lab3_1", Examples.lab3_1, webJars, assets)
class Labyrinth3_2 @Inject()(webJars: WebJarsUtil, assets: Assets)
  extends LabyrinthDebugger("lab3_2", Examples.lab3_2, webJars, assets)
class Labyrinth3_3 @Inject()(webJars: WebJarsUtil, assets: Assets)
  extends LabyrinthDebugger("lab3_3", Examples.lab3_3, webJars, assets)


