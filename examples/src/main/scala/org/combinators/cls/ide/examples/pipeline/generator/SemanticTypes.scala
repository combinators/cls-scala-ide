package org.combinators.cls.ide.examples.pipeline.generator

import org.combinators.cls.types._
import org.combinators.cls.ide.examples.pipeline.PreprocessorStep

object SemanticTypes {
  val PipelineMainClass: Type =  Constructor("PipelineMainClass")
  def DataModifier(fileType: Type): Type = Constructor("DataModifier", fileType)
  val Excel: Type = Constructor("Excel")
  val ClientDataVariable: Type = Constructor("ClientDataVariable")
  val PredictedClientDataVariable: Type = Constructor("PredictedClientDataVariable")
  val VehicleDataVariable: Type = Constructor("VehicleDataVariable")
  val ClientHistoryDataVariable: Type = Constructor("ClientHistoryDataVariable")
  val InstanceDataVariable: Type = Constructor("InstanceDataVariable")
  def EmptyPreprocessor = Constructor("EmptyPreprocessor")
  def Step(step: PreprocessorStep): Type = Constructor(step.getClass.getSimpleName)
  val Solver = Constructor("Solver")
}
