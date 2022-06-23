package org.combinators.cls.ide.examples.pipeline

import controllers.Assets
import org.combinators.cls.ide.examples.pipeline.Demo._
import org.combinators.cls.ide.filter.{Muster, Star, Term}
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.inhabitation.Tree
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Taxonomy, Type}
import org.combinators.cls.ide.examples.pipeline.generator.{Repository, SemanticTypes}
import org.combinators.templating.persistable.PythonWithPath
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.cls.types.syntax._
import org.combinators.cls.ide.examples.pipeline.generator.SemanticTypes.PipelineMainClass

import javax.inject.Inject


class PipelineDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {
  lazy val target = SemanticTypes.PipelineMainClass

  println("start debugger", target)

  lazy val repository = new Repository(pipelineInstance)
  override val controllerAddress = "pipeline"
  override val projectName: String = controllerAddress
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val result = Some(inhabitationResult)
  //  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty,
  //    classLoader = this.getClass.getClassLoader,
  //    algorithm = debugger())


  lazy val inhabitationResult = Gamma.InhabitationBatchJob[PythonWithPath](PipelineMainClass)
    .run()

  lazy val Gamma = repository.reflected

  override val tgts: Seq[Type] = Seq(result.get.target)
  override val nativeType: Option[_] = Some(PythonWithPath)

  override def computeTermsForDownload = {
    val newFilRes = Some(InhabitationResult[PythonWithPath](filteredTreeGraph,
      tgtsFilter, Gamma.evalInhabitant[PythonWithPath]))
    /*if (result.get.isInfinite) {
      val pw =new BufferedWriter(new FileWriter(new File("ResultOrg.txt")))
      for (index <- 0 until 1000){
        val terms = mkTreeMap(Seq(result.get.terms.index(index)))
        pw.write(terms.toString())
        pw.write("\n")

      }
      pw.close()
    }
    */
    /*if (newFilRes.get.isInfinite) {
      val pw =new BufferedWriter(new FileWriter(new File("Filter.txt")))
      for (index <- 0 until 1000){

        println("result in ", newFilRes.get.size)
        val terms = mkTreeMap(Seq(newFilRes.get.terms.index(index)))
        pw.write(terms.toString())
        pw.write("\n")

      }
      pw.close()
      PcrEvaluationUtils(newFilRes.get, withKlarText = true, acceptPercentage).evalInhabitants(0 to 50)
    } else {
      val pw =new BufferedWriter(new FileWriter(new File("Filter.txt")))
      for (index <- 0 until newFilRes.get.size.get.toInt){

        println("result in ", newFilRes.get.size)
        val terms = mkTreeMap(Seq(newFilRes.get.terms.index(index)))
        pw.write(terms.toString())
        pw.write("\n")

      }
      pw.close()
      PcrEvaluationUtils(newFilRes.get, withKlarText = true, acceptPercentage).evalInhabitants(
        0 until newFilRes.get.size.get.toInt)
    }*/
    filteredResult = newFilRes
  }


  def mkTreeMap(trees: Seq[Tree]): Seq[Muster] = {
    var seqTree: Seq[Muster] = Seq.empty
    for (tree <- trees){
      seqTree = seqTree ++ Seq(tree match {
        case Tree(name, _, arguments@_*)=>
          Term(name, mkTreeMap(arguments))
        case _ => Star()
      })
    }
    seqTree
  }
}



