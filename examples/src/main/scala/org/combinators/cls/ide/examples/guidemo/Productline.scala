
package org.combinators.cls.ide.examples.guidemo

import java.nio.file.{Files, Paths}
import javax.inject.Inject

import play.api.mvc.InjectedController
import controllers.Assets
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.git.{EmptyResults, InhabitationController, Results}
import org.combinators.cls.ide.examples.guidemo.Helpers.Form
import org.combinators.cls.ide.inhabitation.TestChannel
import org.combinators.cls.types.Omega
import org.combinators.templating.persistable.JavaPersistable._
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle


class Productline @Inject()(val webJars: WebJarsUtil, val lifeCycle: ApplicationLifecycle, val assets: Assets) extends InhabitationController(webJars, lifeCycle) {
  lazy val projectName = "GuiDemo"
  lazy val repository = new Repository
  lazy val testChannel = new TestChannel

  lazy val Gamma =
    ReflectedRepository(
      repository, substitutionSpace = repository.kinding, classLoader = this.getClass.getClassLoader)


  lazy val combinatorComponents = Gamma.combinatorComponents

  lazy val jobs =
    Gamma.InhabitationBatchJob[Form]('OrderMenu(Omega))
  //Gamma.InhabitationBatchJob[Form]('OrderMenu)

  lazy val results: Results = EmptyResults().addAll(jobs.run())
}

class GUIDemoDataServer() extends InjectedController {
  def logo() = Action {
    Ok(Files.readAllBytes(Paths.get(getClass.getResource("logo.png").toURI))).as("image/png")
  }
  def alternatelogo() = Action {
    Ok(Files.readAllBytes(Paths.get(getClass.getResource("alternatelogo.png").toURI))).as("image/png")
  }
  def productOptions() = Action {
    Ok(s"""["Coffee", "Espresso", "Cappuccino"]""").as("text/plain")
  }
}