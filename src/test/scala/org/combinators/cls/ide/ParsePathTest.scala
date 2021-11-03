
package org.combinators.cls.ide


import org.combinators.cls.ide.inhabitation.{BclDebugger, BoundedCombinatoryLogicDebugger, DebugMsgChannel, FiniteCombinatoryLogicDebugger}
import org.combinators.cls.ide.parser.{NewPathParser, NewRequestParser}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.scalatest.FunSpec



class ParserPathTest extends FunSpec {

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
    tys =>
      tys.foreach {
        case (s, ty) => if (s.length == 0) {
          selection = selection :+ (s, ty)}
      }
  }

  lazy val request = NewRequestParser
  val toCover = Organized(tau).paths.filter(pathInTau => !selection.exists(splitComponent =>
    splitComponent._2.isSubtypeOf(pathInTau)))
  lazy val parsing = NewPathParser

  /*describe("Constructor Parser") {
    it("should parse combinators") {
      assert(parsing.compute(selection.toString()) == List())
    }*/

  val a = Constructor("a")
  val b = Constructor("b")
  val c = Constructor("c")
  val d = Constructor("d")
  val f = Constructor("f")
  val e = Constructor("e")


  describe("Constructor Parser") {
    it("should parse combinators") {
      assert(parsing.compute((List(), "a").toString()).get == (Seq(),a))
    }
    it("should parse parentheses") {
      assert(parsing.compute((List(), (a)).toString()).get == (Seq(),a))
    }
    it("should parse constructor with arguments") {
      assert(parsing.compute((List(),('a ('b, 'c))).toString()).get == (Seq(), Constructor("a", Product(b, c))))
     // assert(parsing.compute((List(),('a('b, 'c, 'd))).toString()) == (Seq(), Constructor("a", Product(Product(Constructor("b"), Constructor("c")), Constructor("d")))))
    }
       it("should parse precedence over products to the left ") {
         assert(parsing.compute((List(),(a <*> b :&: c)).toString()).get == (Seq(), Product(a, Intersection(b, c))))
       }
       it("should take precedence over products to the right ") {
         assert(parsing.compute((List(),(a :&: b <*> c)).toString()).get == (Seq(),Product(Intersection(a, b), c)))
       }
     it("should be right associative") {
       assert(parsing.compute((List(),(a =>: b =>: c)).toString()).get == (Seq(),Arrow(a, Arrow(b, c))))
     }
      it("should be left associative") {
        assert(parsing.compute((List(),(a <*> b <*> c)).toString()).get ==  (Seq(), Product(Product(a, b), c)))
      }
        it("should parse constructors combined with arrows") {
          assert(parsing.compute((List(),('a =>: 'b)).toString()).get == (Seq(),Arrow(a, b)))
          assert(parsing.compute((List(),('a('b) =>: 'c)).toString()).get == (Seq(),Arrow(Constructor("a", b), c)))
          assert(parsing.compute((List(),('a =>: 'b('a, 'c))).toString()).get == (Seq(),Arrow(a, Constructor("b", Product(a, c)))))
          assert(parsing.compute((List(),(a =>: b <*> c)).toString()).get == (Seq(),Arrow(a, Product(b, c))))
          assert(parsing.compute((List(),(a <*> b =>: c)).toString()).get == (Seq(),Arrow(Product(a, b), c)))
        }
       it("should parse constructors combined with intersections") {
         assert(parsing.compute((List(),('a :&: 'b)).toString()).get == (Seq(),Intersection(a, b)))
         assert(parsing.compute((List(),('a('b) :&: 'c)).toString()).get == (Seq(),Intersection(Constructor("a", b), c)))
         assert(parsing.compute((List(),('a :&: 'b('a, 'c))).toString()).get == (Seq(),Intersection(a, Constructor("b", Product(a, c)))))
       }

       it("should pretty print almost identically") {
         //assert(parsing.compute((List(),('a('b, Omega) :&: 'c)).toString) == "List((List(),a(b * omega) & c))")
       }
       it("should test failure"){
         assert(parsing.compute("---") == None)
       }


    it("should parse constructors combined with intersections and an argument") {
      assert(parsing.compute((List(a),(b)).toString()).get == (Seq(a), b))
      assert(parsing.compute((List(a),(b =>: c)).toString()).get == (Seq(a), Arrow(b, c)))
      assert(parsing.compute((List('a('b)),(c)).toString()).get == (Seq(Constructor("a", b)), c))
      assert(parsing.compute((List(a),('b('a,'c))).toString()).get == (Seq(a), Constructor("b", Product(a, c))))
      assert(parsing.compute((List(a),(b <*> c)).toString()).get == (Seq(a), Product(b, c)))
     // assert(parsing.compute((List(a, b),(c)).toString()) == (Seq(Constructor("a"),Constructor("b")), c))
      assert(parsing.compute((List(a),(b =>: c)).toString()).get == (Seq(a), Arrow(b, c)))
      assert(parsing.compute((List(a),(b =>: c) :&: (d =>: e =>: f)).toString()).get == (Seq(a), Intersection(Arrow(b, c), Arrow(d, Arrow(e, f)))))

    }
    it("should parse constructors combined with intersections and two argument") {
      assert(parsing.compute((List(a, b),(c)).toString()).get == (Seq(a, b), Constructor("c")))
    }
    it("should parse constructors combined with intersections and three argument") {
      assert(parsing.compute((List(a,b, c =>: e =>: f),(d)).toString()).get == (Seq(a, b, Arrow(c, Arrow(e, f))), Constructor("d")))
    }
    it("should parse constructors combined with intersections and four argument") {
      assert(parsing.compute((List(a,b, c, e),(d)).toString()).get == (Seq(a, b, c, e), Constructor("d")))
      assert(parsing.compute((List(a,b, c, e =>: f),(d)).toString()).get == (Seq(a, b, c, Arrow(e, f)), Constructor("d")))
    }

  }

}


