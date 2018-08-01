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

  private def withSlash(address: String): String =
    if (address.startsWith("/")) address else s"/$address"

  override def routes: RRoutes = {
    val prefixWithSlash: String =
      routingPrefix.map(withSlash).getOrElse("") ++ withSlash(controllerAddress)

    val directRoutes: RRoutes = {
      case GET(p"/ide") => index()
      case GET(p"/graph") => showGraph()
      case GET(p"/repository") => showRepo()
      case GET(p"/toggleCycle/${int(step)}") => toggleCycles(step)
      case GET(p"/showDebuggerMessages") => showDebuggerMessages()
      case GET(p"/showUninhabitedTy") => showUninhabitedTy()
      case GET(p"/showUnusableCMsg") => showUnusableCMsg()
      case GET(p"/showUnusableBecauseOfTy") => showUnusableBecauseOfTy()
      case GET(p"/steps/${int(step)}") => showSteps(step)
      case GET(p"/computeRequest/${label}") => computeRequest(label)
      //case GET(p"/showResult/${long(index)}") => showResult(index)
      case GET(p"/showPosition/${label}") => showPosition(label)
    }
    new SimpleRouter { def routes: RRoutes = directRoutes }.withPrefix(prefixWithSlash).routes
  }
}
