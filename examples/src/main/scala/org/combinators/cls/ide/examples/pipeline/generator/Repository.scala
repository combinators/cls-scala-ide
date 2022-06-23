package org.combinators.cls.ide.examples.pipeline.generator

import com.github.python3parser.model.{AST, Identifier}
import com.github.python3parser.model.expr.Expression
import com.github.python3parser.model.expr.atoms.trailers.Attribute
import com.github.python3parser.model.expr.atoms.{Atom, Name, Str}
import com.github.python3parser.model.expr.atoms.trailers.arguments.{Arguments, Keyword}
import com.github.python3parser.model.expr.operators.binaryops.comparisons.Eq
import com.github.python3parser.model.mods.Module
import com.github.python3parser.model.stmts.{Body, Statement}
import com.github.python3parser.model.stmts.compoundStmts.{ClassDef, If}
import com.github.python3parser.model.stmts.compoundStmts.functionStmts.FunctionDef
import com.github.python3parser.model.stmts.compoundStmts.functionStmts.parameters.{Parameter, Parameters}
import com.github.python3parser.model.stmts.smallStmts.Pass
import org.combinators.cls.types.syntax._
import org.combinators.cls.inhabitation.InhabitationAlgorithm
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository, combinator}
import org.combinators.cls.types.{FiniteSubstitutionSpace, Omega, Taxonomy, Type}
import org.combinators.templating.persistable.PythonWithPathPersistable._
import org.combinators.templating.persistable.PythonWithPathPersistable
import org.combinators.templating.persistable.PythonWithPath
import org.combinators.cls.ide.examples.pipeline.{CVRPInputGenerator, CVRPMaxTourLength, CheckAndMap, MultipleInputGenerators, Pipeline, ServiceTime, VRPInputGenerator, pipeline}
import org.combinators.cls.ide.examples.pipeline.generator.utilP.Python
import SemanticTypes._
import com.github.python3parser.model.stmts.importStmts.Import
import com.github.python3parser.visitors.modifier.ModifierVisitor
import org.combinators.cls.ide.examples.pipeline.Pipeline
import org.combinators.cls.ide.examples.pipeline.pipeline.FieldName

import java.nio.file.Paths
import java.util
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.language.implicitConversions

trait MainDataModifier {
  def addDataToMainBlock(mainBlock: Body): Unit
  def addArgumentsToSolve(solveArguments: Arguments): Unit
  def addParametersToSolve(solveParameters: Parameters): Unit
  def addParameterReadoutToSolve(solveMethod: Body): Unit
  def addImports(module: Module): Unit
}

trait Preprocessor { self =>
  def addPreprocessorToSolve(solveMethod: Body): Unit
  def addImports(module: Module): Unit

  def andThen(next: Preprocessor): Preprocessor = {
    new Preprocessor {
      def addPreprocessorToSolve(solveMethod: Body): Unit = {
        self.addPreprocessorToSolve(solveMethod)
        next.addPreprocessorToSolve(solveMethod)
      }
      def addImports(module: Module): Unit = {
        self.addImports(module)
        next.addImports(module)
      }
    }
  }
}

trait SolverCode {
  def addSolve(solveMethod: Body): Unit
  def addImports(module: Module): Unit
}

class Repository(val pipelineModel: Pipeline) {

  @combinator object decisionPipeline {
    def apply(solverData: MainDataModifier, preprocessor: Preprocessor, solver: SolverCode): PythonWithPath = {
      val module = new Module()

      solverData.addImports(module)
      preprocessor.addImports(module)
      solver.addImports(module)

      val mainClass = new ClassDef("DecisionPipeline",
        new Arguments(
          mutable.ArrayBuffer[Expression](new Name("object")).asJava,
          Seq.empty.asJava,
          Seq.empty.asJava,
          Seq.empty.asJava,
        ),
        new Body()
      )
      val init = new FunctionDef("__init__", new Parameters(mutable.ArrayBuffer(new Parameter("self")).asJava),
        new Body(Seq[Statement](new Pass()).asJava)
      )
      mainClass.addFunction(init)

      val solveParams = new Parameters(mutable.ArrayBuffer(new Parameter("self")).asJava)
      solveParams.setKwonlyParams(new util.ArrayList[Parameter]())
      solverData.addParametersToSolve(solveParams)
      val solveBody = new Body()
      solveBody.addStatement(Python("""print("Solving our decision pipeline!")""").stmt)
      solverData.addParameterReadoutToSolve(solveBody)
      preprocessor.addPreprocessorToSolve(solveBody)
      solver.addSolve(solveBody)
      val solveMethod = new FunctionDef("solve", solveParams, solveBody)
      mainClass.addFunction(solveMethod)

      val programBlock = new Body()

      solverData.addDataToMainBlock(programBlock)

      programBlock.addStatement(Python("dp = DecisionPipeline()").stmt)

      val solverArgs = new Arguments(new util.ArrayList[Expression](), new util.ArrayList[Keyword](), new util.ArrayList[Expression](), new util.ArrayList[Keyword]())
      solverData.addArgumentsToSolve(solverArgs)
      val solverCall = new Atom(new Name("dp"), Seq(new Attribute(new Identifier("solve")), solverArgs).asJava)
      programBlock.addStatement(solverCall)



      val mainBlock =
        new If(new Eq(new Name("__name__"), new Str("__main__")), programBlock)

      module.addClass(mainClass)
      module.addStatement(mainBlock)

      PythonWithPath(Python(module), Paths.get("decision_pipeline.py"))
    }

    val lastInputGeneratorType: Type = {
      pipelineModel.optimizationInputGenerator match {
        case MultipleInputGenerators(processors) => Step(processors.last)
        //pipeline.MultipleInputGenerators(processors) => Step(processors.last)
      }
    }

    val semanticType: Type = DataModifier(Omega) =>: lastInputGeneratorType =>: Solver =>: PipelineMainClass
  }

  // vehicle_fleet_file, transport_orders_file, historic_service_times_file

  @combinator object clientDataVariable {
    def apply: Name = Python("transport_orders").expression.asInstanceOf[Name]
    val semanticType = ClientDataVariable
  }

  @combinator object predictedClientDataVariable {
    def apply: Name = Python("transport_orders_with_service_time").expression.asInstanceOf[Name]
    val semanticType = PredictedClientDataVariable
  }

  @combinator object vehicleDataVariable {
    def apply: Name = Python("vehicle_fleet").expression.asInstanceOf[Name]
    val semanticType = VehicleDataVariable
  }

  @combinator object clientHistoryDataVariable {
    def apply: Name = Python("historic_service_times").expression.asInstanceOf[Name]
    val semanticType = ClientHistoryDataVariable
  }

  @combinator object instanceDataVariable {
    def apply: Name = Python("inst").expression.asInstanceOf[Name]
    val semanticType = InstanceDataVariable
  }

  object excelDataParameters {
    def apply(clientDataVariableName: Name, vehicleDataVariableName: Name, clientHistoryDataVariableName: Name): MainDataModifier = {
      new MainDataModifier {
        val clientFile = "CLIENT_NEW_FILE"
        val clientHistoricFile = "CLIENT_HISTORIC_FILE"
        val vehicleFile = "VEHICLE_FILE"

        val vehicleParamName = "vehicle_fleet_file"
        val clientParamName = "transport_orders_file"
        val clientHistoryParamName = "historic_service_times_file"

        override def addDataToMainBlock(mainBlock: Body): Unit = {
          mainBlock.addStatement(Python(s"""$clientFile = "${pipelineModel.currentCustomerInputData.path.toString}" """).stmt)
          if (pipelineModel.historicCustomerInputData.isDefined) {
            mainBlock.addStatement(Python(s"""$clientHistoricFile = "${pipelineModel.historicCustomerInputData.get.path.toString}" """).stmt)
          }
          mainBlock.addStatement(Python(s"""$vehicleFile = \"${pipelineModel.vehicleInputData.path.toString}" """).stmt)
          ()
        }

        def addArgumentsToSolve(solveArguments: Arguments): Unit = {
          val args = solveArguments.getKeywords
          args.add(new Keyword(Python(clientParamName).expression, Python(clientFile).expression))
          if (pipelineModel.historicCustomerInputData.isDefined) {
            args.add(new Keyword(Python(clientHistoryParamName).expression, Python(clientHistoricFile).expression))
          }
          args.add(new Keyword(Python(vehicleParamName).expression, Python(vehicleFile).expression))
        }

        def addParametersToSolve(solveParameters: Parameters): Unit = {
          val params = solveParameters.getKwonlyParams
          params.add(new Parameter(clientParamName))
          if (pipelineModel.historicCustomerInputData.isDefined) {
            params.add(new Parameter(clientHistoryParamName))
          }
          params.add(new Parameter(vehicleParamName))
        }

        def addParameterReadoutToSolve(solveMethod: Body): Unit = {
          solveMethod.addStatement(Python(
              s"""${Python(clientDataVariableName).getCode} = DataConnector.read_dataframe_from_excel(${clientParamName})"""
            ).stmt
          )
          if (pipelineModel.historicCustomerInputData.isDefined) {
            solveMethod.addStatement(Python(
                s"""${Python(clientHistoryDataVariableName).getCode} = DataConnector.read_dataframe_from_excel(${clientHistoryParamName})"""
              ).stmt
            )
          }

          solveMethod.addStatement(Python(
              s"""${Python(vehicleDataVariableName).getCode} = DataConnector.read_dataframe_from_excel(${vehicleParamName})"""
            ).stmt
          )
        }

        def addImports(module: Module): Unit = {
          module.addStatement(Python("""import DataConnector""").stmt)
        }
      }
    }
    val semanticType = ClientDataVariable =>: VehicleDataVariable =>: ClientHistoryDataVariable =>: DataModifier(Excel)
  }




  @combinator object emptyPreprocessor {
    def apply(): Preprocessor = new Preprocessor {
      def addPreprocessorToSolve(solveMethod: Body): Unit = ()
      def addImports(module: Module): Unit = ()
    }

    val semanticType: Type = EmptyPreprocessor
  }

  class CheckAndMapPreprocessor(predecessorType: Type, config: CheckAndMap) {
    def apply(predecessor: Preprocessor, clientDataVariableName: Name, vehicleDataVariableName: Name, clientHistoryDataVariableName: Name): Preprocessor = {
      val thisStep = new Preprocessor {
        def addPreprocessorToSolve(solveMethod: Body): Unit = {
          def checkAndMapStep(dataName: String, inputs: Map[FieldName, FieldName], targetVariable: Name): Unit = {
            val mandatory = s"${dataName}_mandatory_field_names"
            solveMethod.addStatement(Python(
              s"""${mandatory} = ${inputs.keys.map(k => s"'$k'").mkString("[", ", ", "]")}"""
            ).stmt)

            val renameMap =
              inputs
                .filter { case (k, v) => k != v }
                .toSeq
                .map { case (k, v) => s"'$k' : '$v'" }
            val rename = s"${dataName}_field_name_map"
            if (renameMap.nonEmpty) {
              solveMethod.addStatement(Python(
                s"""${rename} = ${renameMap.mkString("{", ", ", "}")}"""
              ).stmt)
            }

            solveMethod.addStatement(Python(
              s"""DataConnector.check_and_map(${Python(targetVariable).getCode}, ${mandatory}, ${if (renameMap.isEmpty) "{}" else rename})"""
            ).stmt)
          }

          checkAndMapStep("orders", pipelineModel.currentCustomerInputData.fields, clientDataVariableName)
          if (pipelineModel.historicCustomerInputData.isDefined) {
            checkAndMapStep("hist", pipelineModel.historicCustomerInputData.get.fields, clientHistoryDataVariableName)
          }
          checkAndMapStep("vehicle", pipelineModel.vehicleInputData.fields, vehicleDataVariableName)
        }

        def addImports(module: Module): Unit = {
          var found = false
          val visitor = new ModifierVisitor[Unit] {
            override def visitImport(importElement: Import, param: Unit): AST = {
              if (importElement.getNames.asScala.exists(imp => imp.getName.getName.equals("DataConnector"))) {
                found = true
              }
              importElement
            }
          }
          module.accept(visitor, ())
          if (!found) {
            module.addStatement(Python("""import DataConnector""").stmt)
          }
        }
      }

      predecessor.andThen(thisStep)
    }

    val semanticType: Type = predecessorType =>: ClientDataVariable =>: VehicleDataVariable =>: ClientHistoryDataVariable =>: Step(config)
  }

  class ServiceTimePreprocessor(predecessorType: Type, config: ServiceTime) {
    def apply(predecessor: Preprocessor, clientDataVariableName: Name, predictedClientDataVariable: Name, clientHistoryDataVariableName: Name): Preprocessor = {
      val thisStep = new Preprocessor {
        def addPreprocessorToSolve(solveMethod: Body): Unit = {
          val stmts = Python(
            s"""
               |print("Predicting service time!")
               |pred = service_time_predictor.ServiceTimePredictor()
               |predictors = ${pipelineModel.currentCustomerInputData.fields.keys.map(k => s"'$k'").mkString("[", ", ", "]")}
               |service_time = ['client_service_time']
               |pred.train(${Python(clientHistoryDataVariableName).getCode}, predictors, service_time)
               |
               |## make prediction for new data
               |${Python(predictedClientDataVariable).getCode} = pred.predict_and_merge_dataframe(${Python(clientDataVariableName).getCode}, predictors, service_time)
               |print(${Python(predictedClientDataVariable).getCode}.head(10))""".stripMargin
          ).stmts

          stmts.foreach(solveMethod.addStatement)
        }

        def addImports(module: Module): Unit = {
          module.addStatement(Python("""import service_time_predictor""").stmt)
        }
      }
      predecessor.andThen(thisStep)
    }


    val semanticType: Type = predecessorType =>: ClientDataVariable =>: PredictedClientDataVariable =>: ClientHistoryDataVariable =>: Step(config)
  }

  class VRPPreprocessor(predecessorType: Type, config: VRPInputGenerator) {
    def apply(predecessor: Preprocessor): Preprocessor = {
      predecessor.andThen(emptyPreprocessor.apply())
    }

    val semanticType: Type = predecessorType =>: Step(config)
  }

  class CVRPPreprocessor(predecessorType: Type, config: CVRPInputGenerator) {
    def apply(predecessor: Preprocessor, clientDataVariable: Name, instanceDataVariable: Name, vehicleDataVariable: Name): Preprocessor = {
      val thisStep = new Preprocessor {
        def addPreprocessorToSolve(solveMethod: Body): Unit = {
          val stmts =
            Python(
              s"""
                |### Generate instance object for cvrptl
                |ig = cvrptl_input_generator.CVRP_Input_Generator_Client_Vehicle(${Python(clientDataVariable).getCode}, ${Python(vehicleDataVariable).getCode})
                |${Python(instanceDataVariable).getCode} = ig.get_data()
                |print(inst)""".stripMargin).stmts
          stmts.foreach(solveMethod.addStatement)
        }

        def addImports(module: Module): Unit = {
          module.addStatement(Python("""import cvrptl_input_generator""").stmt)
        }
      }
      predecessor.andThen(thisStep)
    }

    val clientVariableSemantic: Type = {
      pipelineModel.optimizationInputGenerator match {
        case MultipleInputGenerators(processors) if processors.exists { case ServiceTime() => true; case _ => false } =>
          PredictedClientDataVariable
        case _ => ClientDataVariable
      }
    }

    val semanticType: Type = predecessorType =>: clientVariableSemantic =>: InstanceDataVariable =>: VehicleDataVariable =>: Step(config)
  }

  class CVRPSolver(solverConfig: CVRPMaxTourLength) {
    def apply(instanceDataVariable: Name): SolverCode = new SolverCode {
      def addSolve(solveMethod: Body): Unit = {
        val stmts =
          Python(
            s"""
               |solver = cvrptl.CVRPTL()
               |
               |s = solver.optimize(${Python(instanceDataVariable).getCode})
               |if s:
               |    s.print()
               |else:
               |    print("No solution found")""".stripMargin).stmts

        stmts.foreach(solveMethod.addStatement)

        if (solverConfig.visualize) {
          val visualization =
            Python(
              s"""
                 |### Visualize solution (directly opens png in IDE, we could also save an image instead)
                 |if s:
                 |    vis = cvrptl_visualizer.CVRP_Visualizer()
                 |    vis.visualize_solution_and_show(${Python(instanceDataVariable).getCode}, s)
                 |""".stripMargin
            ).stmts
          visualization.foreach(solveMethod.addStatement)
        }
      }

      def addImports(module: Module): Unit = {
        module.addStatement(Python("""import cvrptl""").stmt)
        if (solverConfig.visualize) {
          module.addStatement(Python("""import cvrptl_visualizer""").stmt)
        }
      }
    }
    val semanticType: Type = InstanceDataVariable =>: Solver
  }

  lazy val reflected: ReflectedRepository[Repository] = {

    val repo = ReflectedRepository[Repository](this, classLoader = getClass.getClassLoader)
    println("pipeline", pipelineModel)
    val withInputData =
      if (pipelineModel.currentCustomerInputData.path.toString.endsWith("xlsx")) {
        repo.addCombinator(excelDataParameters)
      } else {
        repo
      }

    val withPreprocessors =
      pipelineModel.optimizationInputGenerator match {
        case MultipleInputGenerators(processors) =>
          processors.foldLeft[(ReflectedRepository[Repository], Type)]((withInputData, EmptyPreprocessor)) {
            case ((r, lastType), c@CheckAndMap()) => (r.addCombinator(new CheckAndMapPreprocessor(lastType, c)), Step(c))
            case ((r, lastType), c@ServiceTime()) => (r.addCombinator(new ServiceTimePreprocessor(lastType, c)), Step(c))
            case ((r, lastType), c@CVRPInputGenerator()) => (r.addCombinator(new CVRPPreprocessor(lastType, c)), Step(c))
            case ((r, lastType), c@VRPInputGenerator()) => (r.addCombinator(new VRPPreprocessor(lastType, c)), Step(c))
            case (s, _) => s
          }._1
      }
    val withSolver =
      pipelineModel.optimizationGoal match {
        case cvrpConfig@CVRPMaxTourLength(_) => withPreprocessors.addCombinator(new CVRPSolver(cvrpConfig))
        case _ => withPreprocessors
      }

    withSolver
  }

  def runInhabitation(): Unit = {
    val result =
      reflected.InhabitationBatchJob[PythonWithPath](PipelineMainClass)
        .run()
    PythonWithPathPersistable[PythonWithPath].persistOverwriting(
      Paths.get("target", "generated"), result.interpretedTerms.index(0)
    )
    ()
  }

}