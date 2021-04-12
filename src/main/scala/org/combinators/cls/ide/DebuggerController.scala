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


import akka.http.impl.util.JavaAccessors.HttpEntity
import akka.stream.javadsl.FileIO
import akka.util.ByteString
import controllers.Assets
import org.combinators.cls.ide.filter.{FilterApply, Star, StarPattern}
import org.combinators.cls.ide.inhabitation._
import org.combinators.cls.ide.parser.{NewFilterParser, NewPathParser, NewRequestParser}
import org.combinators.cls.ide.translator.{ApplicativeTreeGrammarToTreeGrammar, Apply, Combinator, Rule, TreeGrammarToApplicativeTreeGrammar}
import org.combinators.cls.inhabitation._
import org.combinators.cls.interpreter._
import play.api.libs.concurrent.Execution.Implicits._

import java.io.File
import java.nio.file.Paths
import scala.io.Source
//import org.combinators.cls.smt.{GrammarToModel, ParallelInterpreterContext}
import org.combinators.cls.types.{SubtypeEnvironment, _}
import org.webjars.play.WebJarsUtil
import play.api.libs.json.{JsValue, Json, OWrites, Writes}
import play.api.mvc._
import play.filters.csrf._
//import org.combinators.cls.smt.examples.sort.SortExperimentSmtImpl
import scala.collection.mutable


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
  //var model: Option[GrammarToModel] = None
  val filter = new FilterApply()
  val translatorToApplyRules = new TreeGrammarToApplicativeTreeGrammar()
  val translatorToTreeGrammar = new ApplicativeTreeGrammarToTreeGrammar()


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

  case object ApplyNode extends Style

  case object TargetNode extends Style

  case object UninhabitedTypeNode extends Style

  case class Node(label: String, hiddenLabel: String, style: Style, parent: Option[String] = None, id: String = java.util.UUID.randomUUID().toString)

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
    // case InvisibleCombinatorNode => Json.toJson[String]("invisible-unusable-combinator-node")
    // case InvisibleTypeNode => Json.toJson[String]("invisible-uninhabited-type-node")
    case ArgumentNode => Json.toJson[String]("argument-node")
    case TargetNode => Json.toJson[String]("target-node")
    case ApplyNode => Json.toJson[String]("apply-node")
    case UninhabitedTypeNode => Json.toJson[String]("uninhabited-type-node")
  }

  /**
    * Generates a hypergraph
    *
    * @param treeGrammar         computes tree grammar
    * @param tgts                target types
    * @param uninhabitedTypes    types that can not be inhabited
    * @param cannotUseCombinator combinators that can not be used because of their types
    * @return graph
    */
  def toGraph(treeGrammar: TreeGrammar, tgts: Set[Type], uninhabitedTypes: Set[Type], cannotUseCombinator: Set[(String, Seq[Type])]): Graph = {
    val uninhabitedTypeNode: Map[Type, Node] = uninhabitedTypes.map { ty => ty -> Node(ty.toString, ty.toString(),UninhabitedTypeNode) }.toMap
    val tgtNodes: Map[Type, Node] = tgts.map { ty => ty -> Node(ty.toString, ty.toString,TargetNode) }.toMap
    val typeNodes: Map[Type, Node] =
      treeGrammar
        .map { case (ty, _) =>
          var tyName: String= ty.toString()
          if(tyName.contains("!") ){
            tyName = "..."+ty.toString().split("! ")(1)
          }
          ty -> Node(tyName, ty.toString(),TypeNode)
        }
    val allNodes: Map[Type, Node] = tgtNodes ++ uninhabitedTypeNode ++ typeNodes
    val (combinatorNodes, edgeTo, (_, edges)): (Seq[Node], Seq[Edge], (Seq[Node], Seq[Edge])) =
      treeGrammar
        .flatMap { case (ty, r) =>
          r.map { case (c, args) =>
            val combinatorNode = Node(c, c, if (cannotUseCombinator.contains((c, args))) UnusableCombinatorNode else CombinatorNode)
            val edgeTo = Edge(allNodes(ty).id, combinatorNode.id, null) // scalastyle:off
            val argsTyNode = args.zipWithIndex map { case (ty, pos) =>
              val node = Node(ty.toString(), ty.toString(),TypeNode, Some(combinatorNode.id))

              val edgeFrom = Edge(combinatorNode.id, allNodes(ty).id, pos.toString)
              (node, edgeFrom)
            }
            (combinatorNode, edgeTo, argsTyNode)
          }
        }.toSeq.unzip3 match {
        case (x, xt, xs) => (x, xt, xs.flatten.unzip)
      }
    allNodes.map(e => if (e._2.style == UninhabitedTypeNode) warnings = warnings + e._1)
    Graph((allNodes.values ++ combinatorNodes).map(FullNode).toSeq, (edgeTo ++ edges).map(FullEdge))
  }

def toBinaryGraph(treeGrammar: Set[Rule], tgts: Set[Type], uninhabitedTypes: Set[Type], cannotUseCombinator: Set[(String, Seq[Type])]): Graph = {
    val uninhabitedTypeNode: Map[Type, Node] = uninhabitedTypes.map { ty => ty -> Node(ty.toString, ty.toString,UninhabitedTypeNode) }.toMap
    val tgtNodes: Map[Type, Node] = tgts.map { ty => ty -> Node(ty.toString, ty.toString(),TargetNode) }.toMap

    val typeNodes: Map[Type, Node] =
      treeGrammar.map(rule => rule match
      { case Combinator(ty, _) => ty -> Node(ty.toString, if(ty.toString().contains("!")) "hallo" else ty.toString(),TypeNode)
        case Apply(ty, _, _) => ty -> Node(ty.toString, if(ty.toString().contains("!")) "hallo" else ty.toString(),TypeNode)
      }).toMap
    val allNodes: Map[Type, Node] = tgtNodes ++ uninhabitedTypeNode ++ typeNodes
    val (combinatorNodes, edgeTo, (argsTy, edges)): (Seq[Node], Seq[Edge], (Seq[Node], Seq[Edge])) =
      treeGrammar
        .map { case Combinator(ty, r) =>
          // r.map { case (c, args) =>
          val combinatorNode = Node(r, //if (cannotUseCombinator.contains((r, _))) UnusableCombinatorNode else
            r,CombinatorNode)
          val edgeTo = Edge(allNodes(ty).id, combinatorNode.id, null) // scalastyle:off
          // Todo: remove args !!!
          val args: Seq[Type] = Seq.empty
          val argsTyNode = args.zipWithIndex map { case (ty, pos) =>
            val node = Node(ty.toString(), ty.toString() ,TypeNode, Some(combinatorNode.id))

            val edgeFrom = Edge(combinatorNode.id, allNodes(ty).id, pos.toString)
            (node, edgeFrom)
          }
          (combinatorNode, edgeTo, argsTyNode)
        case Apply(ty, fType, argType) =>
          val combinatorNode = Node(s"@", s"@",ApplyNode)
          val edgeTo = Edge(allNodes(ty).id, combinatorNode.id, null)
          val nodeFType = Node(fType.toString(), fType.toString(),TypeNode, Some(combinatorNode.id))
          val edgeFrom = Edge(combinatorNode.id, allNodes(fType).id, "0")
          val nodeArgType = Node(argType.toString(), argType.toString(),TypeNode, Some(combinatorNode.id))
          val edgeFromArgs = Edge(combinatorNode.id, allNodes(argType).id, "1")
          val argsTyNode: Seq[(Node, Edge)] = Seq.empty//, Seq(nodeArgType, edgeFromArgs)))
        val argsTyNode1 = argsTyNode :+ ((nodeFType, edgeFrom)) :+ ((nodeArgType, edgeFromArgs))
          (combinatorNode, edgeTo, argsTyNode1)
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
          val typeNodeSuper = Node(superTy, superTy,TypeNode)
          val typeNodeSubTys = subTy.map { sub =>
            val subNode = Node(sub, sub,SubTypeNode)
            val edgeFrom = Edge(subNode.id, typeNodeSuper.id, null)
            (subNode, edgeFrom)
          }
          (typeNodeSuper, typeNodeSubTys)
        }.toSeq.unzip match {
        case (x, xs) => (x, xs.flatten.unzip)
      }
    Graph((subType ++ superTy).map(FullNode), edges.map(FullEdge))
  }

  /**
    * Returns the domain specified repository
    *
    * @return repository with variables
    */
  def showRepo: Action[AnyContent] = Action {
    if (combinators.nonEmpty) {
      Ok("\u2023 " + combinators.mkString("\n" + "\u2023 "))
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
      Ok("\u2023 " + bcl.get.repository.mkString("\n" + "\u2023 "))
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
  def showPosition(label: String): Action[AnyContent] = Action {
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
  def showUninhabitedTy(): Action[AnyContent] = Action {
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
      val htmlMessage = s"""<lu>${messages.map(e => s"""<li>$e</li>""").mkString}</lu>"""
      Ok(htmlMessage)
    }
  }

  /**
    * Returns messages for unusable combinators
    */
  def showUnusableCMsg(): Action[AnyContent] = Action {
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
      val htmlMessage = s"""<lu>${message.map(e => s"""<li>$e</li>""").mkString}</lu>"""
      Ok(htmlMessage)
    }
  }

  private def showDebuggerMessage() = {
    debugMsgChannel.debugOutput
  }

  /**
    * Returns warnings if the specification of semantic and native types is not correct
    * For example, when the number of semantic types is not equal to the one of native types.
    */
  def showWarnings: Action[AnyContent] = Action {
    if (warnings.isEmpty) {
      Ok("No warnings!")
    } else {
      val htmlMessage = s"""<lu>${warnings.map(e => s"""<li>$e</li>""").mkString}</lu>"""
      Ok(htmlMessage)
    }
  }

  /**
    * @return all types that cannot be inhabiteted
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
    * @return all unused combinators is there are any
    */
  def showUnusedCombinators: Action[AnyContent] = Action {
    if (debugMsgChannel.debugOutput.nonEmpty) {
      val newSet = debugMsgChannel.debugOutput.collect {
        //   case CannotInhabitType(ty) => s"""Type <b>$ty</b> cannot be inhabited! \n"""
        case CannotUseCombinator(combinatorName, tgt, uninhabitedAgrs) =>
          //Todo handle "head"
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
    *
    * @param selectedComb the user select a combinator
    * @return paths for the selected combinator
    */
  def computeNumberOfArgs(selectedComb: String): Action[AnyContent] = Action {
    selectedCombinator = selectedComb
    val splittedRepo: Map[String, Seq[Seq[(Seq[Type], Type)]]] = getSplitRepository
    var radioNumbers: Set[Int] = Set()
    splittedRepo.foreach {
      case (combName, paths) => if (combName == selectedComb) paths.flatten.foreach {
        case (args, _) => radioNumbers += args.length
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
    *
    * @param numbOfArgs The selected number of the arguments
    * @return paths
    */
  def showPaths(numbOfArgs: Int): Action[AnyContent] = Action {
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
    val htmlArgs =
      s"""${
        newPaths.map(e =>
          if (toCover(e).isEmpty) {
            s"""<input class="form-check-input" type= "checkbox" name="checkToCover" value="$e" disabled> $e"""
          }
          else {
            s"""<input class="form-check-input" type= "checkbox" name="checkToCover" value="$e"> $e"""
          }
        ).mkString("\n")
      }"""
    //println("xxx", htmlArgs)
    Ok(htmlArgs)
  }

  /**
    * Computes the paths for target type tau
    *
    * @return paths
    */
  def showOrganizedTy(): Action[AnyContent] = Action {
    //Todo handle "head"
    val orgTy = Organized(newTargets.head).paths
    Ok(orgTy.mkString("\n"))
  }


  /**
    * Computes all paths for the selected combinator
    *
    * @param selected selected combinator
    * @return all paths
    */

  def showToCover(selected: String): Action[AnyContent] = Action {
    var newRequest = selected.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    val newSelection: Option[(Seq[Type], Type)] = NewPathParser.compute(newRequest)
    val toCoverIs: Seq[Type with Path] = toCover(newSelection.get)
    val str =
      s"""${
        toCoverIs.map(e =>
          s"""<li name="$selected"> $e </li>""").mkString("\n")
      }"""
    Ok(str)
  }

  /**
    * Computes the missing paths to cover
    *
    * @param sel selected path
    * @return to cover path
    */
  def toCover(sel: (Seq[Type], Type)): Seq[Type with Path] = {
    val subt = bcl.get.algorithm.subtypes
    import subt._
    //Todo handle "head"
    val prob = Organized((if (newTargets.isEmpty) tgts else newTargets).head).paths.filter(pathInTau => !sel._2.isSubtypeOf(pathInTau))
    //println("ssss", subt)
    //  val prob = Organized(newTargets.head).paths.filter(pathInTau => !sel._2.isSubtypeOf(pathInTau))
    prob
  }

  /**
    * Computes the graph for the step-wise visualisation
    *
    * @param step step to create a graph
    * @return Graph with the targets for the current step
    */
  def showSteps(step: Int): Action[AnyContent] = Action {
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
      if (tgts.isEmpty) {
        Ok("No more steps!")
      } else {
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
    bcl.get.algorithm.inhabitRec((if (newTargets.isEmpty) tgts else newTargets): _*)
  }

  /**
    * Toggles all unproductive cycles and provides a clean view
    *
    * @param step current step
    * @return graph without unproductive cycles
    */
  def toggleCycles(step: Int): Action[AnyContent] = Action {
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
        case (_, arg) =>
          arg
      }
      )
      val newGrammarWithoutTypes = prunedGrammar.filterNot {
        case (ty, _) =>
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
  def showGraph: Action[AnyContent] = Action {
    newGraph = computeResults(reposit, tgts)
    if (newGraph.nonEmpty) {
      graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
      Ok(graphObj.toString)
    } else {
      Ok("Inhabitant not found!")
    }
  }
  /**
    * Returns a result overview as an applicative tree grammar
    */
  def showAppGraph = Action {
    newGraph = computeResults(reposit, tgts)
    newGraph.nonEmpty match {
      case true =>
        //graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        val treeGrammarTo = new TreeGrammarToApplicativeTreeGrammar
        val applicativeTreeGrammar = treeGrammarTo.translateTGtoATG(newGraph)
        graphObj = Json.toJson[Graph](toBinaryGraph(applicativeTreeGrammar, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)
      case false =>
        Ok("Inhabitant not found!")
    }
  }

  /**
    * Returns an inhabitants
    */
  def showResult(index: Int): Action[AnyContent] = Action {
    try {
      val res = result.get.terms.index(index)
      Ok(res.toString)
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }

  /**
    * Returns an interpreted terms, if there is an interpretation
    */
  /*def showInterpretation(index: Int): Action[AnyContent] = Action {
    //println("hallo interpret")
    val filename = "jts_out_inhabitant_000.xml"
    try {
      val file: String = "CAM_CLS_out/20210304_1641/jts_out_inhabitant_000.xml"
     // println("res", result.get.interpretedTerms)
     // val res = result.get.interpretedTerms.values(0)
     val source = Source.fromFile(file).getLines()
      println("res", source)
      Ok(source.mkString())
    } catch {
      case _: Throwable => Ok(s"Inhabitant ${index} not found!")
    }
  }*/


  def aktDir(): String = {
    val dir: File = new File(s"CAM_CLS_out")
   dir.listFiles.sorted
    .filter(_.isDirectory)(0).getName
  }

  def getSolutionsSize(index:Int) = Action {
  val strIndex = padLeadingZeros(index)
    val dir = aktDir()
    val size = Option(new File(s"CAM_CLS_out/${dir}/${strIndex}").list).map(_.filter(_.endsWith(".p")).size).getOrElse(0)
    Ok(size.toString)
}
  def showInterpretation(solution:Int, index: Int) = Action(parse.temporaryFile) {
    implicit request =>
      val strSol = padLeadingZeros(solution)
      val dir = aktDir()
      val file = new java.io.File(s"CAM_CLS_out/${dir}/${strSol}/${index}.p")
       Ok.sendFile(file)
  }
  def getXMLInterpretation(index: Int) = Action(parse.temporaryFile) {
    implicit request =>
      val strIndex = padLeadingZeros(index)
      val dir = aktDir()
      val file = new java.io.File(s"CAM_CLS_out/${dir}/jts_out_inhabitant_${strIndex}.xml")
       Ok.sendFile(file)
  }
  def padLeadingZeros(num:Int): String = {
    val size = 3
    var s = num+""
    while (s.length < size) s = "0" + s
    s
  }
  /**
    * @return the number of computed inhabitants or a message if there are infinitely many inhabitants
    */
  def countsSolutions(): Action[AnyContent] = Action {
    val results = if (result.get.isInfinite) "The result is infinite! How many solutions should be shown?" else result.get.size.get
    Ok(results.toString)
  }

  /**
    * Generates a graph for a taxonomy
    *
    * @return a graph of taxonomy relation
    */
  def showTaxonomyGraph: Action[AnyContent] = Action {
    val graph = toTaxonomyGraph(refRepo.get.semanticTaxonomy.underlyingMap)
    graphObj = Json.toJson[Graph](graph)
    Ok(graphObj.toString())
  }

  /**
    * Represents the defined taxonomies
    *
    * @return the taxonomy specifications if there are any.
    */
  def showTaxonomy: Action[AnyContent] = Action {
    val taxonomy = refRepo.get.semanticTaxonomy.underlyingMap
    if (taxonomy.nonEmpty) {
      Ok(taxonomy.head._1)
    } else {
      Ok("")
    }
  }

  /**
    * @return the number of taxonomies
    */

  def getTaxonomySize: Action[AnyContent] = Action {
    val size = refRepo.get.semanticTaxonomy.underlyingMap.size
    Ok(size.toString)
  }


  /**
    * Generates a graph for an given inhabitant
    *
    * @param index number of inhabitant
    * @return a graph for selected inhabitant
    */
  def inhabitantToGraph(index: Int): Action[AnyContent] = Action {
    try {
      val allPartGrammars: mutable.Set[TreeGrammar] = mutable.Set.empty
      allPartGrammars.clear()
      val partTree: Seq[Tree] = Seq(result.get.terms.index(index))

      def mkTreeMap(trees: Seq[Tree]): TreeGrammar = {
        var partTreeGrammar: Map[Type, Set[(String, Seq[Type])]] = Map()
        trees.map {
          t =>
            val c: (String, Seq[Type]) = (t.name, t.arguments.map(c => c.target))
            val in: TreeGrammar = Map(t.target -> Set(c))
            allPartGrammars.add(in)
            partTreeGrammar = allPartGrammars.toSet.flatten.groupBy(_._1).mapValues(_.flatMap(_._2))
            mkTreeMap(t.arguments)
        }
        partTreeGrammar
      }

      mkTreeMap(partTree)
      newGraph = allPartGrammars.toSet.flatten.groupBy(_._1).mapValues(_.flatMap(_._2))
      graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
      //newGraph = Map()
      Ok(graphObj.toString)
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }


  /**
    * Translates the tree grammar to SMT model
    *
    * @return SMT model
    */
  /* def mkModel: GrammarToModel = {
     val grammar = bcl.get.inhabit(newTargets: _*)
     model =
       Some(GrammarToModel(grammar, newTargets))
     model.get

   }*/

  /**
    * Looks for the combinators that was used for the computation of the tree grammar.
    *
    * @return the used combinators
    */
  def grammarToModel(): Action[AnyContent] = Action {
    /*val model: GrammarToModel = mkModel
    var usedCombinators =
      s"""${
        model.combinatorSeq.map(e =>
          s"""<input class="form-check-input" type= "checkbox" name="optCheckSMT" value ="${model.getIndexForCombinatorName(e)}"> $e </label>""").mkString("\n")
      }"""
    if (usedCombinators.isEmpty) {
      usedCombinators = "Inhabitant not found!"
    }
    Ok(usedCombinators)*/
    Ok("smt not implemented")
  }

  /** Searches for inhabitants without certainly combinators
    *
    * @param combinatorNames Combinators used for the filtering
    * @return If there is an inhabitant, the inhabitant, if not a message "No inhabitant found!"
    */
  def inhabitantsWithoutCombinator(combinatorNames: Seq[Int]): Action[AnyContent] = Action {
    /*var smtResult = ""
    val exContext = ParallelInterpreterContext(model.get)
    for (c <- combinatorNames) {
      val inhabitant: Option[Tree] = Assertions().filterCombinators(c, exContext)
      smtResult = inhabitant match {
        case Some(tree) => tree.toString
        case None => "No inhabitant found!" // Without difference between "unknown" and "unsat"
      }
    }
    Ok(smtResult)*/
    Ok("smt not implemented")
  }

  /* def reachable(treeGrammar: TreeGrammar, target: Type): Boolean = {
     lazy val groundTypes = bcl.get.algorithm.groundTypesOf(treeGrammar, Set(target))
     treeGrammar.foldLeft[TreeGrammar](Map.empty) {
       case (g, (tgt, vs)) =>
         val pruned = vs.filter {
           case (c, args) if (!args.forall(groundTypes)) =>
             false
           case _ =>
             true
         }
         if (pruned.isEmpty) {
           g
         } else {

           g + (tgt -> pruned)
         }
     }
     true
     }*/
  def filterRequest(muster: String): Action[AnyContent] = Action {
    Ok(computeFilterTarget(muster).toString())
  }

  private def computeFilterTarget(pattern: String): Type = {
    var newMuster = pattern.replaceAll("91", "[")
    newMuster = newMuster.replaceAll("93", "]")

    //Todo handle "head"
    var nameTgt = tgts.head
    if (newMuster.contains("|")) {
      val result = newMuster.split('|')
      nameTgt = Constructor(s"{${result.toSeq.reverse.mkString("!")}}! ${nameTgt.toString()}")
    }
    else {
      nameTgt = Constructor(s"${NewFilterParser.compute(newMuster).get}! ${nameTgt.toString()}")
    }
    nameTgt
  }

  /**
    * Computes a new request
    *
    * @param muster for filtering
    */
  def filterMuster(muster: String): Action[AnyContent] = Action {
    var newMuster = muster.replaceAll("91", "[")
    newMuster = newMuster.replaceAll("93", "]")
    val filterGraph: TreeGrammar = Map()
    var newTgtsFilter = computeFilterTarget(muster)
    val appTree = translatorToApplyRules.translateTGtoATG(newGraph)
    if (newMuster.contains("|")) {
      //println("if")
      val result = newMuster.split('|')
      for (i <- result.indices) {
        val pattern = filter.translatePatternToApply(NewFilterParser.compute(result(i)).get)
        newGraph = translatorToTreeGrammar.translateATGtoTG(
          filter.prune(filter.forbidApply(appTree,
            pattern)))
      }
    } else {
      //println("else")
      val parsed = NewFilterParser.compute(newMuster)
      //println("parsed ", parsed)
      val pattern = filter.translatePatternToApply(parsed.get)
      //println("pattern", pattern)
      //nameTgt = s"p! ${nameTgt}"
      newGraph = translatorToTreeGrammar.translateATGtoTG(filter.forbidApply(appTree,
        pattern))
      // filterGraph = bcl.get.algorithm.prune(filterGraph, Set(newTgtsFilter))
    }
    newGraph = bcl.get.algorithm.prune(newGraph, Set(newTgtsFilter))

    val newres = InhabitationResult[Unit](newGraph, newTgtsFilter, _ => ())
    //println("<<<<<xxxx>>>>>", filterGraph)
    //val newest = filterGraph.filterNot(e => e._1.toString().contains("Term"))
    //println("<<<<<1111>>>>>", newres)
    //println("<<<<<1111>>>>>", newTgtsFilter)
    //println("<<<<<xxxx>>>>>", bcl.get.algorithm.groundTypesOf(newGraph,Set(newTgtsFilter)))

    filter.forbidApply(appTree,
      filter.translatePatternToApply(NewFilterParser.compute(muster).get))
    if (newres.isInfinite) {
      for (i <- 0 to 5) {
        //println("iii", newres.terms.index(i))
      }
    } else {
      for (i <- 0 until newres.size.get.toInt) {
      }

    }
    if (newGraph.nonEmpty) {
      graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
      Ok(graphObj.toString)
    } else {
      Ok("Inhabitant not found!")
    }





    /*  if(newMuster.contains("|")){
        val result =  newMuster.split('|')
        newMuster1 = newMuster.split('|').head
        newMuster2 = newMuster.split('|').tail.head

        println("xxxx", result.size)
      }
      if(newMuster2.equals("")){
        val parsedMuster = NewFilterParser.compute(newMuster)
        newGraph = filter.forbid(newGraph, parsedMuster.get)
      }else{
        println("xxxx", newMuster2)
        println("xxx", newMuster)
        val parsedMuster = NewFilterParser.compute(newMuster1)
        newGraph = filter.forbid(newGraph, parsedMuster.get)
        val parsedMuster2 = NewFilterParser.compute(newMuster2)
        newGraph = filter.forbid(newGraph,parsedMuster2.get)
      }
      //newGraph = computeResults(reposit, newTargets)
      var newTgtsFilter = Constructor(s"p! ${tgts.head}")
      if(!newMuster2.equals("")){
        newTgtsFilter = Constructor(s"p! p! ${tgts.head}")
      }

      val prunedFilteredGrammar = bcl.get.algorithm.prune(newGraph, Set(newTgtsFilter))
      println("xxxx", prunedFilteredGrammar)
      val newres = InhabitationResult[Unit](prunedFilteredGrammar, newTgtsFilter, x => ())
      println("<<<<<xxxx>>>>>", newTgtsFilter)
      println("<<<<<xxxx>>>>>", newres.isEmpty)
      println("<<<<<xxxx>>>>>", newres.isInfinite)
      if(newres.isInfinite){
        for(i <- 0 to 5){
          println("iii", newres.terms.index(i))
        }
      }else{
        for ( i <- 0 to newres.size.get.toInt-1){
          println("nnnn", newres.terms.index(i))
        }

      }

      prunedFilteredGrammar.nonEmpty match {
        case true =>
          graphObj = Json.toJson[Graph](toGraph(prunedFilteredGrammar, Set.empty, Set.empty, Set.empty))
          Ok(graphObj.toString)
        case false => Ok("Inhabitant not found!")
      }
      */
  }

  /**
    * Computes a new request
    *
    * @param request new target
    * @return graph if there is a solution or a message
    */
  def computeRequest(request: String): Action[AnyContent] = Action {
    debugMsgChannel.reset()
    var newRequest = request.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    newTargets = NewRequestParser.compute(newRequest)
    newGraph = computeResults(reposit, newTargets)
    if (newGraph.nonEmpty) {
      graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
      Ok(graphObj.toString)
    } else {
      Ok("Inhabitant not found!")
    }
  }

  /**
    * Renders an overview page
    *
    * @return the html code of the page
    */
  def index(): Action[AnyContent] = Action { request =>
    val targets: Seq[Type] =
      try {
        if (newTargets.isEmpty) tgts else newTargets
      }
      catch {
        case _: NullPointerException => tgts
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


