package org.combinators.cls.ide.examples.pipeline

import org.combinators.cls.ide.examples.pipeline.generator.Repository
import org.combinators.cls.ide.examples.pipeline.{CVRPMaxTourLength, InputData, Pipeline, pipeline}

import java.nio.file.Paths

object Demo extends App {
  val clientHistory =
    InputData(
      path = Paths.get("resources", "clients_euc_historic.xlsx"),
      fields = pipeline.defaultHistoricClientMap  + (pipeline.amountFieldName -> "string1")
    )

  val clientCurrent =
    InputData(
      path = Paths.get("resources", "clients_euc_new.xlsx"),
      fields = pipeline.defaultClientMap
    )

  val vehicles =
    InputData(
      path = Paths.get("resources", "vehicles_euc.xlsx"),
      fields = pipeline.defaultCVRPVehicleMap
    )

 /* val optimizerConfig =
    VRPORTools(
      heuristicsConfig = Paths.get("resources", "new?.txt")
    )
*/
  val pipelineInstance =
    Pipeline(
      currentCustomerInputData = clientCurrent,
      historicCustomerInputData = Some(clientHistory),
      vehicleInputData = vehicles,
      optimizationGoal = CVRPMaxTourLength(),
      optimizer = pipeline.standardVPORTools
    )

  val errors = pipelineInstance.findConfigurationErrors
  /*if (errors.nonEmpty) {
    errors.foreach(println)
  } else {

    val repository = new Repository(pipelineInstance)
    println("RRRRR", repository.reflected)
    repository.reflected.combinators.foreach {
      case (combinator, tpe) => println(s"$combinator : $tpe")
    }

    val result = repository.runInhabitation()
      println("result", result)
  }*/
}