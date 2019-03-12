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

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try

class DebuggerController(webjarsUtil: WebJarsUtil, assets: Assets) extends InjectedController {


  /*(val webjarsUtil: WebJarsUtil, val assets: Assets,
                            val combinators: Map[String, (Type, String)], val substitutionSpace: FiniteSubstitutionSpace,
                            val subtypes: SubtypeEnvironment, var targets: Seq[Type], val infinite: Boolean,
                            val results: Results, val testChannel: TestChannel, val projectName: String) extends InjectedController {


      def this(webjarsUtil: WebJarsUtil, assets: Assets,
               substitutionSpace: FiniteSubstitutionSpace,
               subtypes: SubtypeEnvironment, targets: Seq[Type], infinite: Boolean,
               combinatorComponents: Map[String, CombinatorInfo], results: Results, testChannel: TestChannel, projectName: String) =
        this(webjarsUtil, assets, Debugger.toCombinatorsWithDeclarationInfo(combinatorComponents), substitutionSpace, subtypes, targets, infinite, results, testChannel, projectName)

      def this(webjarsUtil: WebJarsUtil, assets: Assets,
               substitutionSpace: FiniteSubstitutionSpace,
               combinators: Map[String, Type], subtypes: SubtypeEnvironment, targets: Seq[Type], infinite: Boolean,
               results: Results, testChannel: TestChannel, projectName: String) =
        this(webjarsUtil, assets, combinators.mapValues(ty => (ty, "")), substitutionSpace, subtypes, targets, infinite, results, testChannel, projectName)


      def this(webjarsUtil: WebJarsUtil, assets: Assets, reflectedRepository: ReflectedRepository[_], results: Results, testChannel: TestChannel,  projectName: String) =
        this(
          webjarsUtil = webjarsUtil,
          assets = assets,
          combinators = reflectedRepository.combinators,
          substitutionSpace = reflectedRepository.substitutionSpace,
          subtypes = SubtypeEnvironment(reflectedRepository.nativeTypeTaxonomy.addNativeTypes().toSet).taxonomy.merge(reflectedRepository.semanticTaxonomy).underlyingMap),
          targets = results.targets.map(_._1),
          infinite = results.infinite,
          results = results,
          testChannel = testChannel,
          projectName = projectName)*/

  var combinatorComponents: Map[String, CombinatorInfo] = Map()
  var newGraph: TreeGrammar = Map()
  var graphObj: JsValue = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
  val infinite: Boolean = true
  var newTargets: Seq[Type] = Seq()
  var combinators: Repository = Map()
  val projectName: String = ""
  var bcl: BoundedCombinatoryLogicDebugger = null
  var debugMsgChannel = new DebugMsgChannel
  var refRepo: ReflectedRepository[_] = null
  lazy val result: InhabitationResult[Unit] = InhabitationResult[Unit](newGraph, newTargets.head, x => ())
  var repo: Map[String, Type] = Map()
  var combinatorName = ""

  def apply(): InhabitationAlgorithm = {
    BoundedCombinatoryLogicDebugger.algorithm(debugMsgChannel)
  }

  /**
    * Generates a tree grammar
    */
  private def inhabitResult(tgt: Seq[Type]): TreeGrammar = {
    debugMsgChannel.reset()
    newGraph = refRepo.algorithm.apply(FiniteSubstitutionSpace.empty,
      SubtypeEnvironment(Map.empty), combinators).apply(tgt)
    newGraph
  }

  def computeResults(Gamma: ReflectedRepository[_], target: Seq[Type], repository: Option[Map[String, Type]] = None) = {
    refRepo = Gamma
    newTargets = target
    combinatorComponents = Gamma.combinatorComponents
    repo = repository match {
      case Some(x) => x
      case None => infoToString.map {
        case (name, (ty, _)) => (name, ty)
      }
    }
    lazy val infoToString = DebuggerController.toCombinatorsWithDeclarationInfo(combinatorComponents)
    val subSpace = Gamma.substitutionSpace
    newGraph = Gamma.algorithm.apply(
      subSpace,
      SubtypeEnvironment(Map.empty),
      repo).apply(newTargets)
    showDebuggerMessage().foreach { case BclDebugger(b, _, _, re, _) =>
      bcl = b
      combinators = re

    case _ =>
    }
    newGraph
  }

  // create Graph
  trait Style

  case object TypeNode extends Style

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
            val edgeTo = Edge(allNodes(ty).id, combinatorNode.id, null)
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
    Graph((allNodes.values ++ combinatorNodes).map(FullNode).toSeq, (edgeTo ++ edges).map(FullEdge))
  }


  /**
    * Returns the repository
    */
  def showRepo: Action[AnyContent] = Action {
    var repo = ""
    var newRepo: Set[String] = Set.empty
    if (combinators.nonEmpty) {
      for ((name, ty) <- combinators) {
        repo = s"$name: $ty"
        newRepo += repo
      }
      Ok(newRepo.mkString("\n"))
    }
    else Ok("Empty Repository")
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
  def showDebuggerMessages: Action[AnyContent] = Action {
    if (debugMsgChannel.debugOutput.nonEmpty) {
      val newSet = debugMsgChannel.debugOutput.collect {
        case CannotInhabitType(ty) => s"""Type <b>$ty</b> cannot be inhabited! \n"""
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
    * Show the Combinator types
    */
  /*def showPaths = Action {
    //println("rep", repo)
    var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]]  = Map()
    var newSplit = Map()

    if (splittedRepo.isEmpty) {splittedRepo = repo.mapValues(bcl.algorithm.split)
      for ((name, ty) <- splittedRepo) {
        println("Split",name, ty.flatten.filter(e=> e._2 match {
          case Intersection(s, t) => true
          case _ =>false
        }))
      }}
    else {splittedRepo}
    println("Split",splittedRepo)
    Ok(splittedRepo.mkString("\n"))
  }*/
  def showPaths(comName: String) = Action {
    combinatorName = comName
    /*var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = repo.mapValues(bcl.algorithm.splitsOf)
    var paths: Set[Type] = Set.empty
    splittedRepo.foreach {
      case (name, tys) => if (name == combinatorName) tys.flatten.foreach {
        case (Seq(), ty) =>
          paths += ty
        case _ =>
      }
    }
    var newPath : Set[Type] = Set.empty
    newPath = computePath(paths)
    def computePath(path: Set[Type]): Set[Type] ={
      path.foreach{
        case Intersection(s, t) =>
          newPath += s
          val newP = Set(t)
          computePath(newP)
        case x => newPath += x
      }
      newPath
    }
    println(newPath)
    val htmlArgs = s"""${newPath.map(e =>
      s"""<input class="form-check-input" type= "checkbox" value="$e"> $e""").mkString("\n")}"""

    Ok(htmlArgs)*/
    var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = repo.mapValues(bcl.algorithm.splitsOf)
   val numberOfArgs = 1
    var newPaths: Set[(Seq[Type], Type)] = Set()
    splittedRepo.foreach {
      case (combName, paths) => if (combName == comName) paths.flatten.foreach {
        case (args, ty) => if (args.length == numberOfArgs) {
          val path: (Seq[Type], Type) = (args, ty)
          newPaths += path
        }
      }
    }

  println(newPaths.map(e => e))
    val htmlArgs = s"""${newPaths.map(e => s"""<input class="form-check-input" type= "checkbox" value="$e"> $e""").mkString("\n")}"""
    Ok(htmlArgs)
  }

  def showNumberOfArgs() = Action{

    var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = repo.mapValues(bcl.algorithm.splitsOf)
    var size: Set[Int] = Set()
    splittedRepo.foreach {
      case (combName, paths) => if (combName == combinatorName) paths.flatten.foreach {
        case (args, ty) =>
          size += args.size
      }
    }
    val htmlSize =
      s"""${
        size.map(e =>
          s"""<label class="radio-inline">
      <input type="radio" name="optradio">$e</label>""").mkString
      }"""
    Ok(htmlSize)
  }

  def showOrganizedTy() = Action {

    val orgTy = Organized(newTargets.head).paths
    println("Org ty", orgTy)
    Ok(orgTy.mkString("\n"))
  }


  def showToCover(selected: String) = Action {
    println("Selected", selected)
    val subt = bcl.algorithm.subtypes
    import subt._
    var splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = repo.mapValues(bcl.algorithm.splitsOf)
    var newRequest = selected.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    val newSelection = NewPathParser.compute(newRequest)
    println("NEW", newSelection)
    /*var selection: (Seq[Type], Type) = null
    splittedRepo.foreach {
      case (name, tys) => tys.flatten.foreach {

        case (s, ty) => if (s.nonEmpty) selection =(s, ty)
      }
    }*/

    //Test selection
    var selection: (Seq[Type], Type) = null
    splittedRepo.foreach {
      case (name, tys) => tys.flatten.foreach {
        case (s, ty) => selection = (s, ty)
         // println("Org ty ", Organized(ty))
        //if ((s,ty).toString() == selected) selection =(s, ty)

      }
    }

    /*val toCover = Organized(newTargets.head).paths.filter(pathInTau => !newSelection.exists(splitComponent =>
      splitComponent._2.isSubtypeOf(pathInTau)))*/
    val toCover = Organized(newTargets.head).paths.filter(pathInTau => !newSelection._2.isSubtypeOf(pathInTau))
    println("selection2", selection)
    println("toCover", toCover)

    Ok(toCover.mkString("\n"))
  }

  /**
    * Computes the steps for the step-wise visualisation
    */
  def showSteps(step: Int) = Action {
    val newInhabitStep: Stream[(TreeGrammar, Stream[Stream[Type]])] = bcl.algorithm.inhabitRec(newTargets: _*)
    var uninhabitedTypes: Set[Type] = Set.empty
    var unusableCombinator: Set[(String, Seq[Type])] = Set.empty
    try {
      val (oldGrammar, oldTgts) = newInhabitStep.splitAt(step)._2.head
      val (grammar, tgts) = computeGrammarAndTgts(oldGrammar, oldTgts)
      debugMsgChannel.reset()
      bcl.algorithm.prune(grammar, tgts.toSet.filter(tgt => !grammar.keys.toSeq.contains(tgt)))
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

  /**
    * Toggles all unproductive cycles and provides a clean view
    */
  def toggleCycles(step: Int) = Action {
    val newInhabitStep: Stream[(TreeGrammar, Stream[Stream[Type]])] = bcl.algorithm.inhabitRec(newTargets: _*)
    var uninhabitedTypes: Set[Type] = Set.empty
    var unusableCombinator: Set[(String, Seq[Type])] = Set.empty
    try {
      val (oldGrammar, oldTgts) = newInhabitStep.splitAt(step)._2.head
      val (grammar, tgts) = computeGrammarAndTgts(oldGrammar, oldTgts)
      debugMsgChannel.reset()
      val prunedGrammar = bcl.algorithm.prune(grammar, tgts.toSet.filter(tgt => !grammar.keys.toSeq.contains(tgt)))
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


  /*private def findAllSubtypes(grammar: TreeGrammar, ty: Type): Option[Type] = {
    import bcl.algorithm.subtypes._
    grammar.keys.find(k => k.isSubtypeOf(ty))
  }*/

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
            (bcl.algorithm.substituteArguments(g, tgt, ty), ty +: tgts)
          case None => (g, tgt +: tgts)
        }
    }
  }

  private def findEqualEntries(grammar: TreeGrammar, ty: Type): Option[Type] = {
    val newBcl = bcl
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
    newGraph.nonEmpty match {
      case true =>
        graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)
      case false =>
        Ok("Inhabitant not found!")
    }
  }
  def showUsedCombinators(index: Int) = Action{
    val tree = Seq(result.terms.index(index))
    var com: Seq[String] = Seq.empty
    usedCombinators(tree)
    def usedCombinators(tree: Seq[Tree]): Seq[String] = {
      tree.foreach {
        case t => usedCombinators(t.arguments)//
          com = t.name +: com
      }
      com
    }
    Ok(com.mkString("\n"))
  }
  /**
    * Shows a list of inhabitants
    */
  def showResult(index: Int) = Action {
    val tree: Tree = result.terms.index(index)
    try {
      Ok(tree.toString)
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }

  //Todo: If there are infinitely many inhabitants, the representation is very slow
  //Todo: Idea: Choose the length of the path
  def countsSolutions = Action {
    lazy val results = if (result.isInfinite) "The result is infinite! How many solutions should be shown?" else result.size.get //.raw.values.flatMap(_._2).size - 1
    try
    Ok(results.toString)
  }
  def showNumberOfSolutions(number: Int) = Action{
      Ok(number.toString)
  }

  /**
    * Generates a graph for an inhabitant
    *
    * @param index number of inhabitant
    *
    */
  def inhabitantToGraph(index: Int) = Action {
    var allPartGrammars: mutable.Set[TreeGrammar] = mutable.Set.empty
    allPartGrammars.clear()
    try {
      val partTree: Seq[Tree] = Seq(result.terms.index(index))

      def mkTreeMap(trees: Seq[Tree]): TreeGrammar = {
        var partTreeGrammar: Map[Type, Set[(String, Seq[Type])]] = Map()
        trees.map {
          case t => val c: (String, Seq[Type]) = (t.name, t.arguments.map(c => c.target))
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
    * Computes a new request
    *
    * @param request new target
    */
  def computeRequest(request: String) = Action {
    debugMsgChannel.reset()
    var newRequest = request.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    newTargets = NewRequestParser.compute(newRequest)
    newGraph = computeResults(refRepo, newTargets)
    newGraph.nonEmpty match {
      case true =>
        graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)
      case false => Ok("Inhabitant not found!")
    }
  }

  /**
    * Renders an overview page
    *
    * @return the html code of the page
    */
  def index() = Action { request =>
    Ok(org.combinators.cls.ide.html.main.render(webjarsUtil, assets, newTargets, request.path, projectName))
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
