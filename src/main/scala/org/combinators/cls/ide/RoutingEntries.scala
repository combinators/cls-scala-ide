/*
 * Copyright 2018 Anna Vasileva
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.cls.ide

import play.api.routing.SimpleRouter
import play.api.routing.Router.{Routes => RRoutes}
import play.api.routing.sird._

trait RoutingEntries extends SimpleRouter { self: Debugger =>

  /** An optional prefix relative to which the routing entries will be considered. */
  val routingPrefix: Option[String] = None

  /** The address of the index page for this [[Debugger]] and
    * the prefix of all sub-pages related to it.
    */
  val controllerAddress: String

  override def routes: RRoutes = {
    val directRoutes: RRoutes = {
      case GET(p"/$prefix") if prefix == controllerAddress => index()
      case GET(p"/$prefix/ide") if prefix == controllerAddress =>index()
      case GET(p"/$prefix/graph") if prefix == controllerAddress => showGraph()
      case GET(p"/$prefix/repository") if prefix == controllerAddress => showRepo()
      case GET(p"/$prefix/steps/${int(step)}") if prefix == controllerAddress => showSteps(step)
      case GET(p"/$prefix/toggleCycle/${int(step)}") if prefix == controllerAddress => toggleCycles(step)
      case GET(p"/$prefix/computeRequest/${label}") if prefix == controllerAddress => computeRequest(label)
      case GET(p"/$prefix/showResult/${int(index)}") if prefix == controllerAddress => showResult(index)
      case GET(p"/$prefix/showPosition/${label}") if prefix == controllerAddress => showPosition(label)
      case GET(p"/$prefix/showOnePossibleSolutionGraph/${int(index)}") if prefix == controllerAddress => inhabitantToGraph(index)
      case GET(p"/$prefix/showDebuggerMessages") if prefix == controllerAddress => showDebuggerMessages()
      case GET(p"/$prefix/showUninhabitedTy") if prefix == controllerAddress => showUninhabitedTy()
      case GET(p"/$prefix/showUnusableCMsg") if prefix == controllerAddress => showUnusableCMsg()
    //  case GET(p"/$prefix/showUnusableBecauseOfTy") if prefix == controllerAddress => showUnusableBecauseOfTy()
      case GET(p"/$prefix/countSolutions") if prefix == controllerAddress => countsSolutions()
    }
    routingPrefix match {
      case None => directRoutes
      case Some(prefix) =>
        val prefixWithSlash = if (prefix.startsWith("/")) prefix else s"/$prefix"
        new SimpleRouter { def routes: RRoutes = directRoutes }.withPrefix(prefixWithSlash).routes
    }
  }
}
