
package org.combinators.cls.ide.examples.guidemo

import java.nio.file.{Files, Paths}
import javax.inject.Inject

import play.api.mvc.InjectedController
import controllers.Assets
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.{CombinatorInfo, ReflectedRepository}
import org.combinators.cls.types.{FiniteSubstitutionSpace, Omega, SubtypeEnvironment, Type}
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import HelpersGui._

class GuiDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled
   {

     lazy val repository = new Repository
     override val controllerAddress = "gui"
     override val projectName = controllerAddress
     lazy val target: Type = 'OrderMenu(Omega)

     //Gamma.InhabitationBatchJob[Form]('OrderMenu(Omega))
     //Gamma.InhabitationBatchJob[Form]('OrderMenu)

     lazy val Gamma = ReflectedRepository(repository,
       substitutionSpace = repository.kinding,
       classLoader = this.getClass.getClassLoader,
       algorithm = debugger())

     debugger.computeResults(Gamma, Seq(target))

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