package org.combinators.cls.ide.examples.planning
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.git.{EmptyResults, InhabitationController, Results}
import org.combinators.cls.ide.inhabitation.TestChannel
import org.combinators.cls.interpreter.ReflectedRepository
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.types.syntax._
import Helpers._

class Planning @Inject()(val webJars: WebJarsUtil, val lifeCycle: ApplicationLifecycle, val assets: Assets) extends InhabitationController(webJars, lifeCycle) {
  lazy val projectName = "planning"
  lazy val repository = new Repository
  lazy val testChannel = new TestChannel

  lazy val Gamma =
    ReflectedRepository(
      repository, substitutionSpace = repository.kinding, classLoader = this.getClass.getClassLoader)


  lazy val combinatorComponents = Gamma.combinatorComponents

  lazy val jobs =
    Gamma.InhabitationBatchJob[String]('PrintConstructionDrawings)
  //Gamma.InhabitationBatchJob[Form]('OrderMenu)

  lazy val results: Results = EmptyResults().addAll(jobs.run())
  println("Results", jobs.run())
}
