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

package org.combinators.cls.ide.inhabitation

import org.combinators.cls.inhabitation.{FiniteCombinatoryLogic, Repository, TreeGrammar}
import org.combinators.cls.types.{Arrow, Intersection, SubtypeEnvironment, Type}


class FiniteCombinatoryLogicDebugger(debugChannel: DebugMessage => Unit, subtypes: SubtypeEnvironment, repository: Repository)
  extends FiniteCombinatoryLogic(subtypes = subtypes, repository = repository) {
  self =>


  override def inhabit(targets: Type*): TreeGrammar = {
    val resultGrammar = inhabitRec(targets: _*).last._1
    if (resultGrammar.isEmpty) {
      targets foreach (tgt => inhabitStep(resultGrammar, tgt) match {
        case (newG, _) => (newG, None)
          debugChannel(CannotInhabitType(tgt))
      })}
    val resultGrammarWithAllTargets = targets.foldLeft(resultGrammar)(ensureTargetExistsIfEqualTypePresent)
    prune(resultGrammarWithAllTargets)

  }

  def prune(grammar: TreeGrammar, tgts: Set[Type]): TreeGrammar = {
    lazy val groundTypes = groundTypesOf(grammar, tgts)
    grammar.foldLeft[TreeGrammar](Map.empty) {
      case (g, (tgt, vs)) =>
        val pruned = vs.filter {
          case (c, args) if (!args.forall(groundTypes)) =>
            debugChannel(CannotUseCombinator(c, tgt, args.filter(!groundTypes(_))))
            false
          case _ => true
        }
        if (pruned.isEmpty) {
          debugChannel(CannotInhabitType(tgt))
          g
        } else {
          g + (tgt -> pruned)
        }

    }

  }

  //type MultiArrow = (Seq[Type], Type)
  def splitsOf(ty: Type): Seq[Seq[(Seq[Type], Type)]] = {
    def safeSplit[A](xss: Seq[Seq[A]]): (Seq[A], Seq[Seq[A]]) =
      xss match {
        case Seq() => (List.empty, List.empty)
        case xs +: Seq() => (xs, List.empty)
        case xs +: xsstl => (xs, xsstl)
      }
    def splitRec(ty: Type, srcs: Seq[Type], delta: Seq[Seq[(Seq[Type], Type)]]): Seq[Seq[(Seq[Type], Type)]] = {
      ty match {
        case Arrow(src, tgt) =>
          val (xs, xss) = safeSplit(delta)
          ((src +: srcs, tgt) +: xs) +: splitRec(tgt, src +: srcs, xss)
        case Intersection(sigma, tau) if sigma.isOmega =>
          splitRec(tau, srcs, delta)
        case Intersection(sigma, tau) if tau.isOmega =>
          splitRec(sigma, srcs, delta)
        case Intersection(sigma, tau) =>
          splitRec(sigma, srcs, splitRec(tau, srcs, delta))
        case _ => delta
      }
    }
    if (ty.isOmega) { List.empty }
    else { List((List.empty, ty)) +: splitRec(ty, List.empty, List.empty) }
  }

  def groundTypesOf(grammar: TreeGrammar, tgts: Set[Type]): Set[Type] = {
    def groundStep(previousGroundTypes: Set[Type]): Set[Type] = {
      grammar.foldLeft(previousGroundTypes) {
        case (s, (k, vs))
          if vs.exists { case (_, args) =>
            args.forall(previousGroundTypes)
          } => s + k
        case (s, _) => s
      }
    }
    lazy val groundStream = Stream.iterate(tgts)(groundStep)
    groundStream
      .zip(groundStream.tail)
      .takeWhile{ case (oldTypes, newTypes) => newTypes.size != oldTypes.size }
      .lastOption
      .map(_._2)
      .getOrElse(Set.empty[Type])
  }

  override def prune(grammar: TreeGrammar): TreeGrammar = {
    prune(grammar, Set.empty)
  }

  // TODO nextgen -> TypeNameStatistics: Combinators should have the same arity
}
