
package org.combinators.cls.ide


import org.combinators.cls.ide.inhabitation.{BclDebugger, BoundedCombinatoryLogicDebugger, DebugMsgChannel, FiniteCombinatoryLogicDebugger}
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
          //println("REQ",ty,  request.compute(ty.toString()))
          selection = selection :+ (s, ty)}
      }
  }

  // println("selection", selection)
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
      assert(parsing.compute((List((List(), "a"))).toString) == Seq((Seq(),a)))
    }
    it("should parse parentheses") {
      assert(parsing.compute((List((List(), (a)))).toString) == Seq((Seq(),a)))
    }
    it("should parse constructor with arguments") {
      assert(parsing.compute((List((List(),('a ('b, 'c))))).toString()) == Seq((Seq(), Constructor("a", Product(b, c)))))
      assert((List((List(),('a('b, 'c, 'd))))) == Seq((Seq(), Constructor("a", Product(Product(Constructor("b"), Constructor("c")), Constructor("d"))))))
    }
       it("should parse precedence over products to the left ") {
         assert((List((List(),(a <*> b :&: c)))) == Seq((Seq(), Product(a, Intersection(b, c)))))
       }
       it("should take precedence over products to the right ") {
         assert((List((List(),(a :&: b <*> c)))) == Seq((Seq(),Product(Intersection(a, b), c))))
       }
     it("should be right associative") {
       assert((List((List(),(a =>: b =>: c)))) == Seq((Seq(),Arrow(a, Arrow(b, c)))))
     }
      it("should be left associative") {
        assert((List((List(),(a <*> b <*> c)))) ==  Seq((Seq(), Product(Product(a, b), c))))
      }
        it("should parse constructors combined with arrows") {
          assert(parsing.compute((List((List(),('a =>: 'b)))).toString()) == Seq((Seq(),Arrow(a, b))))
          assert(parsing.compute((List((List(),('a('b) =>: 'c)))).toString()) == Seq((Seq(),Arrow(Constructor("a", b), c))))
          assert(parsing.compute((List((List(),('a =>: 'b('a, 'c))))).toString()) == Seq((Seq(),Arrow(a, Constructor("b", Product(a, c))))))
          assert((List((List(),(a =>: b <*> c)))) == Seq((Seq(),Arrow(a, Product(b, c)))))
          assert((List((List(),(a <*> b =>: c)))) == Seq((Seq(),Arrow(Product(a, b), c))))
        }
       it("should parse constructors combined with intersections") {
         assert(parsing.compute((List((List(),('a :&: 'b)))).toString()) == Seq((Seq(),Intersection(a, b))))
         assert(parsing.compute((List((List(),('a('b) :&: 'c)))).toString()) == Seq((Seq(),Intersection(Constructor("a", b), c))))
         assert(parsing.compute((List((List(),('a :&: 'b('a, 'c))))).toString()) == Seq((Seq(),Intersection(a, Constructor("b", Product(a, c))))))
       }

       it("should pretty print almost identically") {
         assert((List((List(),('a('b, Omega) :&: 'x)))).toString == "List((List(),a(b * omega) & x))")
       }
       it("should test failure"){
         assert(parsing.compute("---") == Seq.empty)
       }


    it("should parse constructors combined with intersections and an argument") {
      assert(parsing.compute((List((List(a),(b)))).toString()) == Seq((Seq(a), b)))
      assert((List((List(a),(b =>: c)))) == Seq((Seq(a), Arrow(b, c))))
      assert(parsing.compute((List((List('a('b)),(c)))).toString()) == Seq((Seq(Constructor("a", b)), c)))
      assert(parsing.compute((List((List(a),('b('a,'c))))).toString()) == Seq((Seq(a), Constructor("b", Product(a, c)))))
      assert((List((List(a),(b <*> c)))) == Seq((Seq(a), Product(b, c))))
      assert((List((List(a,b),(c)))) == Seq((Seq(a,b), c)))
      assert(parsing.compute((List((List(a),(b =>: c)))).toString()) == Seq((Seq(a), Arrow(b, c))))
      //assert(parsing.compute((List((List(a),((b =>: c) :&: (d =>: e =>: f))))).toString()) == Seq((Seq(a), Intersection(Arrow(b, c), Arrow(d, Arrow(e, f))))))

    }
    it("should parse constructors combined with intersections and two argument") {
      assert((List((List(a,b),(c)))) == Seq((Seq(a, b), Constructor("c"))))
    }
    it("should parse constructors combined with intersections and three argument") {
      assert((List((List(a,b, c),(d)))) == Seq((Seq(a, b, c), Constructor("d"))))
    }

  }

}


