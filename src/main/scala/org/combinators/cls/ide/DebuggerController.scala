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


import akka.http.scaladsl.model
import org.combinators.cls.ide.inhabitation._
import org.combinators.cls.inhabitation._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.SubtypeEnvironment
import org.combinators.cls.types._
import org.webjars.play.WebJarsUtil
import play.api.libs.json.{JsValue, Json, OWrites, Writes}
import play.api.mvc._
import controllers.Assets
import org.apache.commons.io.FileUtils
import org.combinators.cls.ide.filter.{FilterList, Muster, Star, Term}
import org.combinators.cls.smt.{GrammarToModel, ModelToTerm, ModelToTree, ParallelInterpreterContext}
//import org.combinators.cls.smt.examples.sort.SortExperimentSmtImpl
import smtlib.Interpreter
import smtlib.trees.Commands.{CheckSat, GetModel, GetUnsatCore}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try


class DebuggerController(webjarsUtil: WebJarsUtil, assets: Assets) extends InjectedController { // scalastyle:off

  var combinatorComponents: Map[String, CombinatorInfo] = Map()
  var newGraph: TreeGrammar = Map()
  var graphObj: JsValue = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
  val infinite: Boolean = true
  var combinators: Repository = Map()
  val projectName: String = ""
  var bcl: Option[BoundedCombinatoryLogicDebugger] = None
  var debugMsgChannel = new DebugMsgChannel
  val tgts: Seq[Type] = Seq()
  var warnings: Set[Type] = Set()
  var newTargets: Seq[Type] = Seq()
  val refRepo: Option[ReflectedRepository[_]] = None
  val nativeType: Option[_] = None
  val result: Option[InhabitationResult[_]] = None
  val reposit: Option[Map[String, Type]] = None
  var repo: Map[String, Type] = Map()
  var combinatorName = ""
  var selectedCombinator: String = ""
  var model: Option[GrammarToModel] = None
  val filter = new FilterList()



  def apply(): InhabitationAlgorithm = {
    BoundedCombinatoryLogicDebugger.algorithm(debugMsgChannel)
  }

  def computeResults(repository: Option[Map[String, Type]] = None, targets: Seq[Type]): TreeGrammar = {
    combinatorComponents = refRepo.get.combinatorComponents
    repo = repository match {
      case Some(x) => x
      case None => infoToString.map {
        case (name, (ty, _)) => (name, ty)
      }
    }
    lazy val infoToString = DebuggerController.toCombinatorsWithDeclarationInfo(combinatorComponents)
    val subSpace = refRepo.get.substitutionSpace

    newGraph = refRepo.get.algorithm.apply(
      subSpace,
      SubtypeEnvironment(Map.empty),
      repo).apply(targets)
    showDebuggerMessage().foreach {
      case BclDebugger(b, _, _, re, _) =>
      bcl = Some(b)
      combinators = re
    case _ =>
    }
    newGraph
  }

  // create Graph
  trait Style

  case object TypeNode extends Style

  case object SubTypeNode extends Style

  case object CombinatorNode extends Style

  case object UnusableCombinatorNode extends Style

  case object InvisibleCombinatorNode extends Style

  case object InvisibleTypeNode extends Style

  case object ArgumentNode extends Style

  case object TargetNode extends Style

  case object UninhabitedTypeNode extends Style

  case class Node(label: String, style: Style, parent: Option[String] = None, id: String = java.util.UUID.randomUUID().toString)

  case class FullNode(data: Node)

  case class Edge(source: String, target: String, label: String, id: String = java.util.UUID.randomUUID().toString)

  case class FullEdge(data: Edge)

  case class Graph(nodes: Seq[FullNode], edges: Seq[FullEdge])

  implicit lazy val nodeFormat: OWrites[Node] = Json.writes[Node]
  implicit lazy val fullNodeFormat: OWrites[FullNode] = Json.writes[FullNode]

  implicit lazy val edgeFormat: OWrites[Edge] = Json.writes[Edge]
  implicit lazy val fullEdgeFormat: OWrites[FullEdge] = Json.writes[FullEdge]

  implicit lazy val graphFormat: OWrites[Graph] = Json.writes[Graph]


  implicit lazy val styleFormat: Writes[Style] = Writes[Style] {
    case TypeNode => Json.toJson[String]("type-node")
    case SubTypeNode => Json.toJson[String]("subType-node")
    case CombinatorNode => Json.toJson[String]("combinator-node")
    case UnusableCombinatorNode => Json.toJson[String]("unusable-combinator-node")
    case InvisibleCombinatorNode => Json.toJson[String]("invisible-unusable-combinator-node")
    case InvisibleTypeNode => Json.toJson[String]("invisible-uninhabited-type-node")
    case ArgumentNode => Json.toJson[String]("argument-node")
    case TargetNode => Json.toJson[String]("target-node")
    case UninhabitedTypeNode => Json.toJson[String]("uninhabited-type-node")
  }

  /**
    * Generates a hypergraph
    * @param treeGrammar computes tree grammar
    * @param tgts target types
    * @param uninhabitedTypes types that can not be inhabited
    * @param cannotUseCombinator combinators that can not be used because of their types
    * @return graph
    */
  def toGraph(treeGrammar: TreeGrammar, tgts: Set[Type], uninhabitedTypes: Set[Type], cannotUseCombinator: Set[(String, Seq[Type])]): Graph = {
    val uninhabitedTypeNode: Map[Type, Node] = uninhabitedTypes.map { ty => ty -> Node(ty.toString, UninhabitedTypeNode) }.toMap
    val tgtNodes: Map[Type, Node] = tgts.map { ty => ty -> Node(ty.toString, TargetNode) }.toMap
    val typeNodes: Map[Type, Node] =
      treeGrammar
        .map { case (ty, _) => ty -> Node(ty.toString, TypeNode)
        }
    val allNodes: Map[Type, Node] = tgtNodes ++ uninhabitedTypeNode ++ typeNodes
    val (combinatorNodes, edgeTo, (argsTy, edges)): (Seq[Node], Seq[Edge], (Seq[Node], Seq[Edge])) =
      treeGrammar
        .flatMap { case (ty, r) =>
          r.map { case (c, args) =>
            val combinatorNode = Node(c, if (cannotUseCombinator.contains((c, args))) UnusableCombinatorNode else CombinatorNode)
            val edgeTo = Edge(allNodes(ty).id, combinatorNode.id, null) // scalastyle:off
          val argsTyNode = args.zipWithIndex map { case (ty, pos) =>
            val node = Node(ty.toString(), TypeNode, Some(combinatorNode.id))

            val edgeFrom = Edge(combinatorNode.id, allNodes(ty).id, pos.toString)
            (node, edgeFrom)
          }
            (combinatorNode, edgeTo, argsTyNode)
          }
        }.toSeq.unzip3 match {
        case (x, xt, xs) => (x, xt, xs.flatten.unzip)
      }
    allNodes.map(e => if (e._2.style == UninhabitedTypeNode) warnings = warnings+ e._1)
    Graph((allNodes.values ++ combinatorNodes).map(FullNode).toSeq, (edgeTo ++ edges).map(FullEdge))
  }

  def toTaxonomyGraph(taxonomy: Map[String, Set[String]]): Graph = {
    val (superTy, (subType, edges)): (Seq[Node], (Seq[Node], Seq[Edge])) =
      taxonomy
        .map { case (superTy, subTy) =>
          val typeNodeSuper =  Node(superTy, TypeNode)
          val typeNodeSubTys = subTy.map { sub =>
            val subNode= Node(sub, SubTypeNode)
            val edgeFrom = Edge(subNode.id, typeNodeSuper.id, null)
            (subNode, edgeFrom)
          }
          (typeNodeSuper, typeNodeSubTys)
      }.toSeq.unzip match {
        case (x, xs) => (x, xs.flatten.unzip)
      }
    Graph((subType ++ superTy).map(FullNode).toSeq, (edges).map(FullEdge))
  }

  /**
    * Returns the domain specified repository
    * @return repository with variables
    */
  def showRepo: Action[AnyContent] = Action {
    if (combinators.nonEmpty) {
      Ok("\u2023 " + combinators.mkString("\n"+"\u2023 "))
    }
    else {
      Ok("Empty Repository")
    }
  }

  /**
    * Returns the repository without variables
    */
  def showRepoWithoutVars: Action[AnyContent] = Action {
    if (bcl.get.repository.nonEmpty) {
      Ok("\u2023 " + bcl.get.repository.mkString("\n"+"\u2023 "))
    }
    else {
      Ok("Empty Repository")
    }
  }

  /**
    * Returns the repository for the covering
    */
  def showRepoCovering: Action[AnyContent] = Action {
    if (combinators.nonEmpty) {
      val radioButtons = s"""${combinators.map(e => s"""<input class = "form-radio" type="radio" name="optradioCovering" value ="${e._1}"> ${e._1}: ${e._2} </label>""").mkString("\n")}"""
      Ok(radioButtons)
    }
    else {
      Ok("Empty Repository")
    }
  }

  /**
    * Shows in the debug overview the implementation of the combinator
    *
    * @param label the chosen combinator
    */
  def showPosition(label: String) = Action {
    var newEntry = ""
    val combinatorToString = DebuggerController.toCombinatorsWithDeclarationInfo(combinatorComponents)

    for ((name, (ty, position)) <- combinatorToString) {
      if (position.contains(label) || ty.toString() == label) {
        newEntry = name + ": " + ty
      }
    }
    Ok(newEntry.mkString(""))
  }

  /**
    * Returns the uninhabited types
    */
  def showUninhabitedTy() = Action {
    val messages = showDebuggerMessage().map {
      case CannotInhabitType(ty) => ty
      case _ =>
    }.toList.filter {
      case () => false
      case _ => true
    }
    if (messages.isEmpty) {
      Ok("No messages.")
    } else {
      val htmlMessage = s"""<lu>${(messages.map(e => s"""<li>$e</li>""")).mkString}</lu>"""
      Ok(htmlMessage)
    }
  }

  /**
    * Returns messages for unusable combinators
    */
  def showUnusableCMsg() = Action {
    val message = showDebuggerMessage().map {
      case CannotUseCombinator(combinatorName, _, _) => combinatorName
      case _ =>
    }.toList.filter {
      case () => false
      case _ => true
    }
    if (message.isEmpty) {
      Ok("No messages.")
    } else {
      val htmlMessage = s"""<lu>${(message.map(e => s"""<li>$e</li>""")).mkString}</lu>"""
      Ok(htmlMessage)
    }
  }

  private def showDebuggerMessage() = {
    debugMsgChannel.debugOutput
  }

  /**
    * Returns debugger messages
    */
  def showWarnings: Action[AnyContent] = Action {
    if (warnings.isEmpty){
      Ok("No warnings!")
    } else {
      val htmlMessage = s"""<lu>${(warnings.map(e => s"""<li>$e</li>""")).mkString}</lu>"""
      Ok(htmlMessage)
    }
  }
  /**
    * Returns debugger messages
    */
  def showUninhabitedTypes: Action[AnyContent] = Action {
    if (debugMsgChannel.debugOutput.nonEmpty) {
      val newSet = debugMsgChannel.debugOutput.collect {
        case CannotInhabitType(ty) => s"""Type <b>$ty</b> cannot be inhabited! \n"""
     /*   case CannotUseCombinator(combinatorName, tgt, uninhabitedAgrs) =>
          s"""Combinator <b>$combinatorName</b> cannot be used with target \n
             <b>$tgt</b> because of type <b>${uninhabitedAgrs.head}</b>!
             """.stripMargin*/
      }
      Ok(newSet.mkString("\n"))
    }
    else {
      Ok("No messages")
    }
  }
  /**
    * Returns debugger messages
    */
  def showUnusedCombinators: Action[AnyContent] = Action {
    if (debugMsgChannel.debugOutput.nonEmpty) {
      val newSet = debugMsgChannel.debugOutput.collect {
     //   case CannotInhabitType(ty) => s"""Type <b>$ty</b> cannot be inhabited! \n"""
        case CannotUseCombinator(combinatorName, tgt, uninhabitedAgrs) =>
          s"""Combinator <b>$combinatorName</b> cannot be used with target \n
             <b>$tgt</b> because of type <b>${uninhabitedAgrs.head}</b>!
             """.stripMargin
      }
      Ok(newSet.mkString("\n"))
    }
    else {
      Ok("No messages")
    }
  }

  /**
    * Computes the number of the arguments of a selected combinator
    * @param selectedComb
    * @return paths for the selected combinator
    */
  def computeNumberOfArgs(selectedComb: String) = Action {
    selectedCombinator = selectedComb
    var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = getSplitRepository
    var radioNumbers: Set[Int] = Set()
    splittedRepo.foreach {
      case (combName, paths) => if (combName == selectedComb) paths.flatten.foreach {
        case (args, ty) => radioNumbers += args.length
      }
    }
    val radioButtons = s"""${radioNumbers.map(e => s"""<input class = "form-radio" type="radio" name="optradio" value ="$e"> $e </label>""").mkString("\n")}"""
    Ok(radioButtons)
  }

  private def getSplitRepository = {
    repo.mapValues(bcl.get.algorithm.splitsOf)
  }

  /**
    * Show the combinator types
    * @param numbOfArgs The selected number of the arguments
    * @return paths
    */
  def showPaths(numbOfArgs: Int) = Action {
    val splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = getSplitRepository
    var newPaths: Set[(Seq[Type], Type)] = Set()
    splittedRepo.foreach {
      case (combName, paths) => if (combName == selectedCombinator) paths.flatten.foreach {
        case (args, ty) => if (args.length == numbOfArgs) {
          val path: (Seq[Type], Type) = (args, ty)
          newPaths += path
        }
      }
    }
    val htmlArgs = s"""${newPaths.map(e =>
      if (toCover(e).isEmpty) {
        s"""<input class="form-check-input" type= "checkbox" name="checkToCover" value="$e" disabled> $e"""}
      else{
        s"""<input class="form-check-input" type= "checkbox" name="checkToCover" value="$e"> $e"""}
    ).mkString("\n")}"""
    Ok (htmlArgs)
  }

  /**
    * Computes the paths for target type tau
    * @return paths
    */
  def showOrganizedTy() = Action {
    val orgTy = Organized(newTargets.head).paths
    Ok(orgTy.mkString("\n"))
  }


  /**
    * Computes all paths for the selected combinator
    * @param selected selected combinator
    * @return all paths
    */

  def showToCover(selected: String) = Action {
    var newRequest = selected.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    val newSelection: Option[(Seq[Type], Type)] = NewPathParser.compute(newRequest)
    val toCoverIs: Seq[Type with Path] = toCover(newSelection.get)
    val str = s"""${toCoverIs.map(e =>
      s"""<li name="$selected"> $e </li>""").mkString("\n")}"""
    Ok(str)
  }

  /**
    * Computes the missing paths to cover
    * @param sel selected path
    * @return to cover path
    */
  def toCover(sel: (Seq[Type], Type)): Seq[Type with Path] = {
    val subt = bcl.get.algorithm.subtypes
    import subt._
   // val prob = Organized((if(newTargets.isEmpty)tgts else newTargets).head).paths.filter(pathInTau => !sel._2.isSubtypeOf(pathInTau))
    val prob = Organized(newTargets.head).paths.filter(pathInTau => !sel._2.isSubtypeOf(pathInTau))
    prob
  }

  /**
    * Computes the graph for the step-wise visualisation
    * @param step step to create a graph
    * @return Graph with the targets for the current step
    */
  def showSteps(step: Int) = Action {
    val newInhabitStep: Stream[(TreeGrammar, Stream[Stream[Type]])] = getInhabitStep
    var uninhabitedTypes: Set[Type] = Set.empty
    var unusableCombinator: Set[(String, Seq[Type])] = Set.empty
    try {
      val (oldGrammar, oldTgts) = newInhabitStep.splitAt(step)._2.head
      val (grammar, tgts) = computeGrammarAndTgts(oldGrammar, oldTgts)
      debugMsgChannel.reset()
      bcl.get.algorithm.prune(grammar, tgts.toSet.filter(tgt => !grammar.keys.toSeq.contains(tgt)))
      unusableCombinator =
        computeUnusableCombinator(grammar)
      tgts.isEmpty match {
        case true => Ok("No more steps!")
        case false =>
          uninhabitedTypes = unusableCombinator.flatMap(_._2)
          graphObj = Json.toJson[Graph](toGraph(grammar, tgts.toSet, uninhabitedTypes, unusableCombinator))
          Ok(graphObj.toString())
      }
    }
    catch {
      case _: NoSuchElementException => Ok("No such element")
    }
  }

  private def getInhabitStep = {
    bcl.get.algorithm.inhabitRec((if(newTargets.isEmpty) tgts else newTargets): _*)
  }

  /**
    * Toggles all unproductive cycles and provides a clean view
    * @param step current step
    * @return graph without unproductive cycles
    */
  def toggleCycles(step: Int) = Action {
    val newInhabitStep: Stream[(TreeGrammar, Stream[Stream[Type]])] = getInhabitStep
    var uninhabitedTypes: Set[Type] = Set.empty
    var unusableCombinator: Set[(String, Seq[Type])] = Set.empty
    try {
      val (oldGrammar, oldTgts) = newInhabitStep.splitAt(step)._2.head
      val (grammar, tgts) = computeGrammarAndTgts(oldGrammar, oldTgts)
      debugMsgChannel.reset()
      val prunedGrammar = bcl.get.algorithm.prune(grammar, tgts.toSet.filter(tgt => !grammar.keys.toSeq.contains(tgt)))
      unusableCombinator =
        computeUnusableCombinator(grammar)

      uninhabitedTypes = unusableCombinator.flatMap(_ match {
        case (com, arg) =>
          arg
      }
      )
      val newGrammarWithoutTypes = prunedGrammar.filterNot {
        case (ty, r) =>
          uninhabitedTypes.contains(ty)
      }
      graphObj = Json.toJson[Graph](toGraph(newGrammarWithoutTypes, tgts.toSet, uninhabitedTypes, unusableCombinator))
      Ok(graphObj.toString())

    } catch {
      case _: NoSuchElementException => Ok("No such element")
    }
  }


  /**
    * Returns tree grammar and the new targets
    */
  private def computeGrammarAndTgts(oldGrammar: TreeGrammar, oldTgts: Stream[Stream[Type]]) = {
    oldTgts.flatten.foldLeft((oldGrammar, Stream.empty[Type])) {
      case ((g, tgts), tgt) =>
        findEqualEntries(g, tgt) match {
          case Some(ty) =>
            debugMsgChannel(SubtypeOf(tgt, ty))
            debugMsgChannel.reset()
            (bcl.get.algorithm.substituteArguments(g, tgt, ty), ty +: tgts)
          case None => (g, tgt +: tgts)
        }
    }
  }

  private def findEqualEntries(grammar: TreeGrammar, ty: Type): Option[Type] = {
    val newBcl = bcl.get
    import newBcl.algorithm.subtypes._

    grammar.keys.find(k => k.isSupertypeOf(ty) && k.isSubtypeOf(ty))
  }

  /**
    * Computes the unusable combinators
    */
  private def computeUnusableCombinator(grammar: TreeGrammar) = {
    grammar.toSeq.flatMap {
      case (ty, options) =>
        options.filter {
          case (c, args) =>
            debugMsgChannel.debugOutput.exists {
              case CannotUseCombinator(uc, tgt, uninhabitedArgs) =>
                (c == uc) && (tgt == ty) && args.exists(uArg => uninhabitedArgs.contains(uArg))
              case _ => false
            }
        }
    }.toSet
  }

  /**
    * Returns a result overview
    */
  def showGraph = Action {
    newGraph = computeResults(reposit, tgts)
    newGraph.nonEmpty match {
      case true =>
        graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)
      case false =>
        Ok("Inhabitant not found!")
    }
  }

  /**
    * Returns an inhabitants
    */
  def showResult(index: Int) = Action {
    try {
      /*val res = if(result.get.size.get == 0){
        InhabitationResult[Unit](newGraph, (if(newTargets.isEmpty) tgts else newTargets).head, x => ()).terms.index(index)
      }else{
        result.get.terms.index(index)
      }*/
      Ok(result.get.terms.index(index).toString)
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }

  //If there are infinitely many inhabitants, the representation is very slow
  def countsSolutions() = Action {
      val results = if (result.get.isInfinite) "The result is infinite! How many solutions should be shown?" else result.get.size.get
    Ok(results.toString)
  }
  def showTaxonomyGraph = Action{
    val graph = toTaxonomyGraph(refRepo.get.semanticTaxonomy.underlyingMap)
    graphObj = Json.toJson[Graph](graph)
    Ok(graphObj.toString())
  }

  def showTaxonomy = Action {
    val taxonomy = refRepo.get.semanticTaxonomy.underlyingMap
    if (taxonomy.size != 0) {
      Ok(taxonomy.head._1.toString())
    }else     {
      Ok("")
    }
  }

  def getTaxonomySize = Action{
    val size = refRepo.get.semanticTaxonomy.underlyingMap.size
    Ok(size.toString)
  }


  /**
    * Generates a graph for an inhabitant
    * @param index number of inhabitant
    * @return a graph for selected inhabitant
    */
  def inhabitantToGraph(index: Int) = Action {
    try {
      var allPartGrammars: mutable.Set[TreeGrammar] = mutable.Set.empty
      allPartGrammars.clear()
      val partTree: Seq[Tree] = Seq(result.get.terms.index(index))

      def mkTreeMap(trees: Seq[Tree]): TreeGrammar = {
        var partTreeGrammar: Map[Type, Set[(String, Seq[Type])]] = Map()
        trees.map {
           t => val c: (String, Seq[Type]) = (t.name, t.arguments.map(c => c.target))
            var in: TreeGrammar = Map(t.target -> Set(c))
            allPartGrammars.add(in)
            partTreeGrammar = allPartGrammars.toSet.flatten.groupBy(_._1).mapValues(_.map(_._2).flatten)
            mkTreeMap(t.arguments)
        }
        partTreeGrammar
      }
      mkTreeMap(partTree)
      newGraph = allPartGrammars.toSet.flatten.groupBy(_._1).mapValues(_.map(_._2).flatten)
      graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
      newGraph = Map()
      Ok(graphObj.toString)
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }

  /**
    * Translates the tree grammar to SMT model
    * @return SMT model
    */
  def mkModel: GrammarToModel = {
    val grammar = bcl.get.inhabit(newTargets: _*)
    model =
      Some(GrammarToModel(grammar, newTargets))
    model.get
  }

  /**
    * Looks for the combinators that was used for the computation of the tree grammar.
    * @return the used combinators
    */
  def grammarToModel() = Action{
    val model: GrammarToModel = mkModel
    var usedCombinators = s"""${model.combinatorSeq.map(e =>
      s"""<input class="form-check-input" type= "checkbox" name="optCheckSMT" value ="${model.getIndexForCombinatorName(e)}"> $e </label>""").mkString("\n")}"""
    if (usedCombinators.isEmpty){
      usedCombinators = "Inhabitant not found!"
    }
    Ok(usedCombinators)
  }

  /** Searches for inhabitants without certainly combinators
    * @param combinatorNames Combinators used for the filtering
    * @return If there is an inhabitant, the inhabitant, if not a message "No inhabitant found!"
    */
  def inhabitantsWithoutCombinator(combinatorNames: Seq[Int]) = Action {
    var smtResult = ""
    val exContext = ParallelInterpreterContext(model.get)
    for(c <- combinatorNames){
    val inhabitant: Option[Tree] = Assertions().filterCombinators(c, exContext)
      smtResult = inhabitant match {
      case Some(tree) => tree.toString
      case None => "No inhabitant found!" // Without difference between "unknown" and "unsat"
      }
    }
    Ok(smtResult)
  }


  /**
    * Computes a new request
    * @param request new target
    */
  def computeRequest(request: String) = Action {
    debugMsgChannel.reset()
    var newRequest = request.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    newTargets = NewRequestParser.compute(newRequest)
    newGraph = computeResults(reposit, newTargets)
    newGraph.nonEmpty match {
      case true =>
        graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)
      case false => Ok("Inhabitant not found!")
    }
  }

  /**
    * Renders an overview page
    * @return the html code of the page
    */
  def index() = Action {request =>
    val targets: Seq[Type] =
      try {
   if (newTargets.isEmpty)tgts else newTargets
    }
    catch {
      case e : NullPointerException => tgts
    }
    Ok(org.combinators.cls.ide.html.main.render(webjarsUtil, assets, targets, request.path, projectName))

  }
}

object DebuggerController {

  def toCombinatorsWithDeclarationInfo(combinatorComponents: Map[String, CombinatorInfo]): Map[String, (Type, String)] =
    combinatorComponents.mapValues {
      case staticInfo: StaticCombinatorInfo =>
        (ReflectedRepository.fullTypeOf(staticInfo),
          s"${scala.reflect.runtime.universe.show(staticInfo.fullSignature)}")
      case dynamicInfo: DynamicCombinatorInfo[_] =>
        (ReflectedRepository.fullTypeOf(dynamicInfo),
          dynamicInfo.position.mkString("\n"))
    }
}


