
package org.combinators.cls.ide.examples.temperature

import javax.inject.Inject
import com.github.javaparser.ast.CompilationUnit
import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git.{EmptyResults, InhabitationController}
import org.combinators.cls.ide.inhabitation.{BoundedCombinatoryLogicDebugger, TestChannel}
import org.combinators.cls.types.syntax._
import org.combinators.templating.persistable.JavaPersistable._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle


class Temperature @Inject()(val webJars: WebJarsUtil, val applicationLifecycle: ApplicationLifecycle, val assets: Assets) extends InhabitationController(webJars, applicationLifecycle) {

  // Note: This will produce two variations; only the first is deemed accurate, and it is interesting
  // to consider how to deny the synthesis of the second one...
  lazy val projectName = "Temperature"
  lazy val repository = new Repository {}


  import repository._

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace=repository.precisions.merge(units), semanticTaxonomy=taxonomyLoss,
    classLoader = this.getClass.getClassLoader)
  lazy val testChannel = new TestChannel
  lazy val reflectedRepository = ReflectedRepository(repository, taxonomyLoss, precisions.merge(units), BoundedCombinatoryLogicDebugger.algorithm(testChannel),
    this.getClass.getClassLoader)
  lazy val combinatorComponents = Gamma.combinatorComponents

  lazy val jobs = Gamma.InhabitationBatchJob[CompilationUnit](artifact('Impl))
  //alternative goals:
  /* :&: precision(precision.floating))
    .addJob[CompilationUnit](artifact(artifact.impl) :&: precision(precision.integer) :&: unit(unit.celsius))*/
  // lazy val jobs = reflectedRepository.InhabitationBatchJob[CompilationUnit](artifact ( artifact.impl ) )
  // lazy val jobs = reflectedRepository.InhabitationBatchJob[com.github.javaparser.ast.CompilationUnit]('Artifact ( 'Impl ) :&: 'Unit ( 'Celsius ))
  lazy val results = EmptyResults().addAll(jobs.run())





  // Omega is like Object -- the base type everything inherits from
  //Gamma.inhabit[JType](precision(Omega))
}


