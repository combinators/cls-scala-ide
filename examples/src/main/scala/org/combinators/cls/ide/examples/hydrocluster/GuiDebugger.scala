package org.combinators.cls.ide.examples.hydrocluster

import controllers.Assets
import javax.inject.Inject
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class GuiDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {

  //testRequest für ExampleRepo1. Hier sagt die unten ausgeführte Inhabitation "No Results", der Debugger kann aber eine
  //Baumgrammatik aufbauen.
  //Fixed in der neuen Version der IDE.
  val testRequest1: SynthesisRequest = SynthesisRequest(
    name = "test",
    reqType = "test",
    height = 2,
    vineSensors = List("Humidity", "Temperature"), //("Humidity", "Temperature"),
    turbineSensors = List("Water"), //List("Humidity", "Temperature"),
    honeycombSensors = List("Uvlevel"),
    towerActuators = List("Lamp"),
    validLayers = List("Vine", "HoneyComb"),
    topSensors = List()
  )

  //testRequest für ExampleRepo2 und ExampleRepo3. ExampleRepo2 hat die ins leere laufenden Pfeile mit den
  //roten Knoten ohne cylces. ExampleRepo3 ist ein funktionierendes Beispiel für ein früheres, deutlich kleineres
  //Repository.
  //
  val testRequest23: SynthesisRequest = SynthesisRequest(
    name = "test",
    reqType = "test",
    height = 2,
    vineSensors = List("Humidity", "Temperature"), //("Humidity", "Temperature"),
    turbineSensors = List("Water"), //List("Humidity", "Temperature"),
    honeycombSensors = List("Uvlevel"),
    towerActuators = List("Lamp"),
    validLayers = List("Vine", "Turbine", "HoneyComb"),
    topSensors = List()
  )

  //testRequest für ExampleRepo4. Hier hat die Baumgrammatik mehrere Terme für semantisch gleiche Interpretationen.
  //Sprich ganz viele Duplikate in den interpretedTerms.
  val testRequest4: SynthesisRequest = SynthesisRequest(
    name = "test",
    reqType = "test",
    height = 2,
    vineSensors = List(), //("Humidity", "Temperature"),
    turbineSensors = List(), //List("Humidity", "Temperature"),
    honeycombSensors = List(),
    towerActuators = List("Lamp"),
    validLayers = List("Vine", "Turbine", "HoneyComb"),
    topSensors = List()
  )

  //Zum Ausprobieren der Beispiele ExampleRepo___ und testRequest___ anpassen.
  lazy val Gamma: ReflectedRepository[ExampleRepo2.Repository] = ExampleRepo2.Repository(testRequest23).toReflectedRepository(debugger(), getClass.getClassLoader)
  lazy val target: Type = 'tower
  override val controllerAddress = "pg"
  override val projectName: String = controllerAddress
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val result = Some(inhabitRes)
  override val tgts: Seq[Type] = Seq(result.get.target)

  val maxResults = 1
  lazy val inhabitRes: InhabitationResult[Algebraic.Algebra] = Gamma.inhabit[Algebraic.Algebra](target)

  def showResult(inhabitRes: InhabitationResult[Algebraic.Algebra], index: Int): Unit = {
  /*  println(index)
    println(inhabitRes.terms.index(index).target)
    println("-------------------------------------------------")
   // println(inhabitRes.interpretedTerms.index(index).result(MetaModel.metaModelTermAlgebra))
    println("-------------------------------------------------")

    println("-------------------------------------------------")
  */}

 /* if (inhabitRes.isEmpty) {println("No Results", inhabitRes)}
  else if (!inhabitRes.isEmpty && !inhabitRes.isInfinite) {
  println("-------------------------------------------------")
    println("Number of Results is finite")
    println(" ------", inhabitRes.target)
    println("-------------------------------------------------")
    /*
    for {
      i <- 0 to maxResults
      if i < inhabitRes.size.get
    } {
      showResult(inhabitRes, i)
    }

    */
  }
  else {
   // println("Infinite results")
   // println("-------------------------------------------------")
    /*
    for {
      i <- 0 to maxResults
    } {
      showResult(inhabitRes, i)
    }

     */
  }
*/

  //debugger.computeResults(Gamma, Seq(target))


}

