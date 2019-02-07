
package org.combinators.cls.ide.inhabitation

import org.combinators.cls.interpreter.ReflectedRepository
import org.scalatest._
import org.combinators.cls.types._

import org.combinators.cls.types.SubtypeEnvironment

class PathTest extends FunSpec {

  val mapTest =
    Map(
      "sigma" -> Intersection(
        Arrow(
          Constructor("a"), Arrow(Constructor("b"),
            Constructor("c"))),
        (Intersection(Arrow(Constructor("d"), Constructor("e")),
          Arrow(Constructor("f"), Intersection(Arrow(Constructor("g"), Constructor("h")), Arrow(Constructor("i"),
          Arrow(Constructor("j"), Constructor("k"))))
          )))))
  val taxonomy: Taxonomy =
    Taxonomy
      .empty
      .merge(Taxonomy("Char")
        .addSubtype("Int"))
      .merge(Taxonomy("String"))

  val tau = Arrow(Intersection(Constructor("b"),
              Intersection(Constructor("g"),
                Constructor("i"))), Intersection(Constructor("c"), Intersection(Constructor("h"), Arrow(Constructor("j"), Constructor("k")))))



  val testChannel = new DebugMsgChannel()
  val GammaFCL = new FiniteCombinatoryLogicDebugger(testChannel, SubtypeEnvironment(taxonomy.underlyingMap), mapTest)

  lazy val GammaBCL = BoundedCombinatoryLogicDebugger.algorithm(testChannel)
  lazy val refRepo = ReflectedRepository(mapTest, taxonomy, FiniteSubstitutionSpace.empty, GammaBCL)
  val subtypeEnvironment = SubtypeEnvironment(taxonomy.underlyingMap)
  val reflectedRepository2 = ReflectedRepository(mapTest, substitutionSpace = FiniteSubstitutionSpace.empty, classLoader = this.getClass.getClassLoader)

  val tree = GammaBCL.apply(Kinding.empty,subtypeEnvironment, mapTest)
  val alsResult = tree.apply(Seq(tau))
  val result = GammaFCL.inhabit(tau)
  val subt = GammaFCL.subtypes
  import subt._

  var typ: Set[Type] = Set()
  val repo= testChannel.debugOutput.map{
    case BclDebugger(_, _, _, repo, _) => repo.map{
      case (name, ty) => typ += ty
        ty
    }
    case _ =>
  }.filter{
    case () => false
    case _ => true
  }

  val splits = GammaFCL.splitsOf(typ.head)

  val orgTau = Organized(tau).paths
  var selection: Seq[(Seq[Type], Type)] = Seq()
  splits.foreach {
    tys => tys.foreach {
      case (s, ty) => if (s.length == 0) selection = selection :+ (s, ty)
    }
  }
  println("selection", selection)
  val toCover = Organized(tau).paths.filter(pathInTau => !selection.exists(splitComponent =>
    splitComponent._2.isSubtypeOf(pathInTau)))

  describe("splitsOf") {
    describe("|- ? : tau") {
      testChannel.reset()
      val splits0Args = (Seq.empty,Intersection(
        Arrow(
          Constructor("a"), Arrow(Constructor("b"),
            Constructor("c"))),
        (Intersection(Arrow(Constructor("d"), Constructor("e")),
          Arrow(Constructor("f"), Intersection(Arrow(Constructor("g"), Constructor("h")), Arrow(Constructor("i"),
            Arrow(Constructor("j"), Constructor("k"))))
          )))))
      val splits1Arg = (Seq(Constructor("a")),Arrow(
          Constructor("b"), Constructor("c")))
      val splits2Arg = (Seq(Constructor("b"), Constructor("a")), Constructor("c"))
      val splits3Arg = (Seq(Constructor("j"), Constructor("i"), Constructor("f")), Constructor("k"))

       it(s"should constrains $splits0Args") {
        assert(splits.flatten.contains(splits0Args))
      }
       it(s"should constrains $splits1Arg") {
         assert(splits.flatten.contains(splits1Arg))
       }
      it(s"should constrains $splits2Arg") {
         assert(splits.flatten.contains(splits2Arg))
       }
      it(s"should constrains $splits3Arg") {
         assert(splits.flatten.contains(splits3Arg))
       }
}}}
