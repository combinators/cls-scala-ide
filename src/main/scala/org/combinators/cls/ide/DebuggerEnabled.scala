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

import play.api.NoHttpFiltersComponents
import play.api.routing.Router.{Routes => RRoutes}
import play.api.routing.SimpleRouter
import play.api.routing.sird._

trait DebuggerEnabled extends SimpleRouter with NoHttpFiltersComponents{self: DebuggerController => //RoutingEntries =>

  val routingPrefix: Option[String] = None
  /**
    * The address of the index page
    */
  val controllerAddress: String


  val debugger: DebuggerController = self


  override def routes: RRoutes = {
    val directRoutes: RRoutes = {
      case GET(p"/$prefix/") if prefix == controllerAddress =>index()
      case GET(p"/$prefix/ide") if prefix == controllerAddress =>index()
      case GET(p"/$prefix/graph") if prefix == controllerAddress => showGraph()
      case GET(p"/$prefix/appGraph") if prefix == controllerAddress => showAppGraph()
      case GET(p"/$prefix/steps/${int(step)}") if prefix == controllerAddress => showSteps(step)
      case GET(p"/$prefix/toggleCycle/${int(step)}") if prefix == controllerAddress => toggleCycles(step)
      case GET(p"/$prefix/computeRequest/${label}") if prefix == controllerAddress => computeRequest(label)
      case GET(p"/$prefix/repository") if prefix == controllerAddress => showRepo()
      case GET(p"/$prefix/repositoryWithoutVars") if prefix == controllerAddress => showRepoWithoutVars()
      case GET(p"/$prefix/smt") if prefix == controllerAddress => grammarToModel()
      case GET(p"/$prefix/filter/${muster}") if prefix == controllerAddress => filterMuster(muster)
      case GET(p"/$prefix/resetFilter") if prefix == controllerAddress => resetFilter()
      case GET(p"/$prefix/filterRequest/${muster}") if prefix == controllerAddress => filterRequest(muster)
      case GET(p"/$prefix/showUnusedCombinators") if prefix == controllerAddress => showUnusedCombinators()
      case GET(p"/$prefix/showUninhabitedTypes") if prefix == controllerAddress => showUninhabitedTypes()
      case GET(p"/$prefix/showWarnings") if prefix == controllerAddress => showWarnings()
      case GET(p"/$prefix/showUninhabitedTy") if prefix == controllerAddress => showUninhabitedTy()
      case GET(p"/$prefix/showUnusableCMsg") if prefix == controllerAddress => showUnusableCMsg()
      case GET(p"/$prefix/showResult/${int(index)}") if prefix == controllerAddress => showResult(index)
      case GET(p"/$prefix/showFilteredResult/${int(index)}") if prefix == controllerAddress => showFilteredResult(index)
      case GET(p"/$prefix/showInterpretation/${int(solution)}/${int(index)}") if prefix == controllerAddress => showInterpretation(solution,index)
      case GET(p"/$prefix/getXMLInterpretation/${int(index)}") if prefix == controllerAddress => getXMLInterpretation(index)
      case GET(p"/$prefix/getSolutionSize/${int(index)}") if prefix == controllerAddress => getSolutionsSize(index)
      //case GET(p"/$prefix/showUsedCombinators/${int(index)}") if prefix == controllerAddress => showUsedCombinators(index)
      case GET(p"/$prefix/countSolutions") if prefix == controllerAddress => countsSolutions()
      case GET(p"/$prefix/countFilteredSolutions") if prefix == controllerAddress => countFilteredSolutions()
      case GET(p"/$prefix/showTaxonomy") if prefix == controllerAddress => showTaxonomy()
      case GET(p"/$prefix/getTaxonomySize") if prefix == controllerAddress => getTaxonomySize()
      case GET(p"/$prefix/showTaxonomyGraph") if prefix == controllerAddress => showTaxonomyGraph()
      case GET(p"/$prefix/showPosition/${label}") if prefix == controllerAddress => showPosition(label)
      case GET(p"/$prefix/showOnePossibleSolutionGraphcy-part-graph/${int(index)}") if prefix == controllerAddress => inhabitantToGraph(index)
      case GET(p"/$prefix/showOnePossibleSolutionGraphcy-filter-graph/${int(index)}") if prefix == controllerAddress => inhabitantToFilteredGraph(index)
      case GET(p"/$prefix/inhabitantsWithoutCombinator/items" ? q_s"tag=${int(tags)}") if prefix == controllerAddress =>
        inhabitantsWithoutCombinator(tags)
      case GET(p"/$prefix/showPaths/${int(args)}") if prefix == controllerAddress => showPaths(args)
      case GET(p"/$prefix/repositoryCovering") if prefix == controllerAddress => showRepoCovering()
      case GET(p"/$prefix/computeNumberOfArgs/${combName}") if prefix == controllerAddress => computeNumberOfArgs(combName)
      case GET(p"/$prefix/showToCover/${selection}") if prefix == controllerAddress => showToCover(selection)
      case GET(p"/$prefix/showOrganizedTy") if prefix == controllerAddress => showOrganizedTy()

    }
    routingPrefix match {
      case None => directRoutes
      case Some(prefix) =>
        val prefixWithSlash = if (prefix.startsWith("/")) prefix else s"/$prefix"
        new SimpleRouter {
          def routes: RRoutes = directRoutes
        }.withPrefix(prefixWithSlash).routes
    }
  }
}
