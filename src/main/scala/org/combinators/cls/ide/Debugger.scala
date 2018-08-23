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

import java.lang

import org.combinators.cls.git.Results
import org.combinators.cls.ide.inhabitation._
import org.combinators.cls.inhabitation._
import org.combinators.cls.interpreter._
import org.combinators.cls.types._
import org.webjars.play.WebJarsUtil
import play.api.libs.json.{JsValue, Json, OWrites, Writes}
import play.api.mvc._
import controllers.Assets
import org.combinators.templating.persistable.Persistable
import shapeless.feat.Enumeration

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class Debugger(val webjarsUtil: WebJarsUtil, val assets: Assets,
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


  lazy val bcl = new BoundedCombinatoryLogicDebugger(testChannel, substitutionSpace, subtypes, combinators.mapValues(_._1))
  var newGraph: TreeGrammar = Map()
  var newTargets: Seq[Type] = targets
  var graphObj: JsValue = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))


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
            var argsTyNode = args.zipWithIndex map { case (ty, pos) =>
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
    val repo = combinators
    if (combinators.nonEmpty) {
      val toShow = repo map {
        case (c, ty) => c + ": " + ty
      }
      Ok(toShow.mkString("\n"))
    }
    else Ok("Empty Repository")
  }

  /**
    * Shows in the debug overview the implementation of the combinator
    * @param label the chosen combinator
    */
  def showPosition(label: String) = Action {
    var newEntry = ""
    for ((name, (ty, position)) <- combinators) {
      if (name == label || ty.toString() == label) {
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
    }
    val newMsg = messages.toList.filter {
      case () => false
      case _ => true
    }
    Ok(newMsg.mkString("\n"))
  }

  def showUnusableBecauseOfTy() = Action {
    val messages = showDebuggerMessage().map {
      case CannotInhabitBacauseOfSubtype(combinatorName, uninhabitedArgs) => combinatorName
      case _ =>
    }
    val newMsg = messages.toList.filter {
      case () => false
      case _ => true
    }
    Ok(newMsg.mkString("\n"))
  }

  /**
    * Returns messages for unusable combinators
    */
  def showUnusableCMsg() = Action {
    val message = showDebuggerMessage().map {
      case CannotUseCombinator(combinatorName, tgt, uninhabitedAgrs) =>
        combinatorName
      case _ =>
    }
    val newMsg = message.toList.filter {
      case () => false
      case _ => true
    }
    Ok(newMsg.mkString("\n"))
  }

  private def showDebuggerMessage(): mutable.Set[DebugMessage] = {
    testChannel.debugOutput
  }

  /**
    * Returns debugger messages
    */
  def showDebuggerMessages: Action[AnyContent] = Action {
    if (testChannel.debugOutput.nonEmpty) {
      val newSet = testChannel.debugOutput.collect {
        case CannotUseCombinator(combinatorName, tgt, uninhabitedAgrs) =>
          s"""|Combinator $combinatorName cannot be inhabited with target
              |$tgt because of type
              |$uninhabitedAgrs
              """.stripMargin
        case CannotInhabitType(ty) => ty
      }
      Ok(newSet.mkString)
    }
    else {
      Ok("No messages")
    }
  }

  def findEqualEntries(grammar: TreeGrammar, ty: Type): Option[Type] = {
    import bcl.algorithm.subtypes._
    grammar.keys.find(k => k.isSupertypeOf(ty) && k.isSubtypeOf(ty))
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
      testChannel.reset()
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
      testChannel.reset()
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

  /**
    * Returns tree grammar and the new targets
    */
  private def computeGrammarAndTgts(oldGrammar: TreeGrammar, oldTgts: Stream[Stream[Type]]) = {
    oldTgts.flatten.foldLeft((oldGrammar, Stream.empty[Type])) {
      case ((g, tgts), tgt) =>
        findEqualEntries(g, tgt) match {
          case Some(ty) => (bcl.algorithm.substituteArguments(g, tgt, ty), ty +: tgts)
          case None => (g, tgt +: tgts)
        }
    }
  }

  /**
    * Computes the unusable combinators
    */
  private def computeUnusableCombinator(grammar: TreeGrammar) = {
    grammar.toSeq.flatMap {
      case (ty, options) =>
        options.filter {
          case (c, args) =>

            testChannel.debugOutput.exists {
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
    newGraph = inhabitResult(newTargets)
    newGraph.nonEmpty match {
      case true =>
        graphObj = Json.toJson[Graph](toGraph(newGraph, Set.empty, Set.empty, Set.empty))
        Ok(graphObj.toString)

      case false =>
        Ok("Inhabitant not found!")

    }
  }

  /**
    * Shows a list of inhabitants
    */
  def showResult(index: Long) = Action {
    try {
      Ok(results.raw.index(index).mkString("\n"))
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $index")
    }
  }

  def countsSolutions = Action {
    lazy val numbers = if (results.infinite) 100 else results.raw.values.flatMap(_._2).size - 1
    Ok(numbers.toString)
  }

  /**
    * Generates a graph for an inhabitant
    * @param index number of inhabitant
    *
    */
  def inhabitantToGraph(index: Long) = Action {
    var allPartGrammars: mutable.Set[TreeGrammar] = mutable.Set.empty
    allPartGrammars.clear()
    try {
      val partTree: Seq[Tree] = results.raw.index(index)
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
    * Generates a tree grammar
    */
  private def inhabitResult(tgt: Seq[Type]): TreeGrammar = {
    bcl.algorithm.inhabit(tgt: _*)
  }

  /**
    * Computes a new request
    * @param request new target
    */
  def computeRequest(request: String) = Action {
    testChannel.reset()
    var newRequest = request.replaceAll("91", "[")
    newRequest = newRequest.replaceAll("93", "]")
    newTargets = NewRequestParser.compute(newRequest)
    newGraph = inhabitResult(newTargets)
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
  def index() = Action { request =>
    Ok(org.combinators.cls.ide.html.main.render(webjarsUtil, assets, combinators, infinite, newTargets, request.path, projectName))
  }
}

object Debugger {
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