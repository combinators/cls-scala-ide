
package org.combinators.cls.ide.inhabitation

import org.combinators.cls.inhabitation.{FiniteCombinatoryLogic, Repository, TreeGrammar}
import org.combinators.cls.types.{SubtypeEnvironment, Type}


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

  override def removeEntriesWithArgument(grammar: TreeGrammar, arg: Type): TreeGrammar =
    grammar.mapValues(entries => entries.filterNot {
      case (c, args) if ((args.contains(arg))) =>
        debugChannel(CannotInhabitBacauseOfSubtype(c, args))
        false
      case _ =>
        true

    })
  // TODO nextgen -> TypeNameStatistics: Combinators should have the same arity
}