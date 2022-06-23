package org.combinators.cls.ide.examples.pipeline

import java.nio.file.{Path, Paths}
import org.combinators.cls.ide.examples.pipeline.pipeline.FieldName

case class Pipeline(
                     currentCustomerInputData: InputData,
                     historicCustomerInputData: Option[InputData],
                     vehicleInputData: InputData,
                     optimizationGoal: Goal,
                     optimizer: OptimizerConfig = pipeline.standardVPORTools,
                     _optimizationInputGenerator: Option[OptimizationInputGeneratorConfig] = None,
                   ) {
  def optimizationInputGenerator: OptimizationInputGeneratorConfig = {
    _optimizationInputGenerator match {
      case Some(config) => config
      case None => optimizationGoal.defaultInputGeneratorConfig
    }
  }

  def findConfigurationErrors: Seq[String] = {
    var errors = Seq.empty[String]
    if (historicCustomerInputData.isDefined) {
      optimizationInputGenerator match {
        case MultipleInputGenerators(preprocessors) if (!preprocessors.exists( { case ServiceTime() => true; case _ => false }) )=>
          errors = errors :+ "Preprocessors need service time predictor if historic client data is present"
        case _ if currentCustomerInputData.fields.keys.exists(k => k == pipeline.serviceTimeFieldName) =>
          errors = errors :+ "If no historic data is given, clients data requires a service time field"
        case _ => ()
      }
    }
    errors
  }
}

case class InputData(
                      path: Path,
                      fields: Map[FieldName, FieldName]
                    )

trait Goal {
  def defaultInputGeneratorConfig: OptimizationInputGeneratorConfig
}
case class CVRPMaxTourLength(visualize: Boolean = true) extends Goal {
  def optimizationInputGeneratorConfig(): OptimizationInputGeneratorConfig = {
    MultipleInputGenerators(
      Seq(
        CheckAndMap(),
        ServiceTime(),
        CVRPInputGenerator()
      )
    )
  }
  override def defaultInputGeneratorConfig: OptimizationInputGeneratorConfig = optimizationInputGeneratorConfig()
}
case class VRPMaxTourLength() extends Goal {
  def optimizationInputGeneratorConfig(): OptimizationInputGeneratorConfig =
    MultipleInputGenerators(
      Seq(
        CheckAndMap(),
        ServiceTime(),
        VRPInputGenerator()
      )
    )
  override def defaultInputGeneratorConfig: OptimizationInputGeneratorConfig = optimizationInputGeneratorConfig()
}

trait OptimizationInputGeneratorConfig

case class MultipleInputGenerators(
                                    processors: Seq[PreprocessorStep]
                                  ) extends OptimizationInputGeneratorConfig

trait PreprocessorStep
case class CheckAndMap() extends PreprocessorStep
case class ServiceTime() extends PreprocessorStep
case class CVRPInputGenerator() extends PreprocessorStep
case class VRPInputGenerator() extends PreprocessorStep

trait OptimizerConfig
case class VRPORTools(
                       heuristicsConfig: Path
                     ) extends OptimizerConfig

package object pipeline {
  val clientLongitudeFieldName: FieldName = "client_longitude"
  val clientLatitutdeFieldName: FieldName = "client_latitude"
  val serviceTimeFieldName: FieldName = "client_service_time"
  val amountFieldName: FieldName = "client_amount"
  val vehicleLongitudeFieldName: FieldName = "vehicle_longitude"
  val vehicleLatitudeFieldName: FieldName = "vehicle_latitude"
  val vehicleMaximumTourLengthFieldName: FieldName = "vehicle_max_tour_length"
  val vehicleCapacityFieldName: FieldName = "vehicle_capacity"

  val defaultClientMap = Map(
    clientLongitudeFieldName -> clientLongitudeFieldName,
    clientLatitutdeFieldName -> clientLatitutdeFieldName,
    amountFieldName -> amountFieldName
  )

  val defaultHistoricClientMap = defaultClientMap + (serviceTimeFieldName -> serviceTimeFieldName)

  val defaultVRPVehicleMap = Map(
    vehicleLongitudeFieldName -> vehicleLongitudeFieldName,
    vehicleLatitudeFieldName -> vehicleLatitudeFieldName,
    vehicleMaximumTourLengthFieldName -> vehicleMaximumTourLengthFieldName
  )
  val defaultCVRPVehicleMap = defaultVRPVehicleMap + (vehicleCapacityFieldName -> vehicleCapacityFieldName)


  val standardVPORTools = VRPORTools(Paths.get("default.conf"))
  type FieldName = String
}

