package org.combinators.cls.ide


import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.scalatest.FunSpec
import scala.util.parsing.combinator.Parsers



class ParserTest extends FunSpec {

  val a = Constructor("a")
  val b = Constructor("b")
  val c = Constructor("c")
  val parsing = NewRequestParser

  describe("Constructor Parser") {
    it("should parse combinators") {
      assert(parsing.compute("a") == Seq(a))
    }
    it("should parse parentheses") {
      assert(parsing.compute("(a)") == Seq(a))
    }
    it("should parse constructor with arguments") {
      assert(parsing.compute(('a ('b, 'c)).toString()) == Seq(Constructor("a", Product(b, c))))
      assert('a('b, 'c, 'd) == Constructor("a", Product(Product(Constructor("b"), Constructor("c")), Constructor("d"))))
    }
    it("should parse precedence over products to the left ") {
      assert((a <*> b :&: c) == Product(a, Intersection(b, c)))
    }
    it("should take precedence over products to the right ") {
      assert((a :&: b <*> c) == Product(Intersection(a, b), c))
    }
    it("should be right associative") {
      assert((a =>: b =>: c) == Arrow(a, Arrow(b, c)))
    }
    it("should be left associative") {
      assert((a <*> b <*> c) == Product(Product(a, b), c))
    }
    it("should parse constructors combined with arrows") {
      assert(parsing.compute(('a =>: 'b).toString()) == Seq(Arrow(a, b)))
      assert(parsing.compute(('a('b) =>: 'c).toString()) == Seq(Arrow(Constructor("a", b), c)))
      assert(parsing.compute(('a =>: 'b('a, 'c)).toString()) == Seq(Arrow(a, Constructor("b", Product(a, c)))))
      assert((a =>: b <*> c) == Arrow(a, Product(b, c)))
      assert((a <*> b =>: c) == Arrow(Product(a, b), c))
    }
    it("should parse constructors combined with intersections") {
      assert(parsing.compute(('a :&: 'b).toString()) == Seq(Intersection(a, b)))
      assert(parsing.compute(('a('b) :&: 'c).toString()) == Seq(Intersection(Constructor("a", b), c)))
      assert(parsing.compute(('a :&: 'b('a, 'c)).toString()) == Seq(Intersection(a, Constructor("b", Product(a, c)))))
    }

    it("should pretty print almost identically") {
      assert(('a('b, Omega) :&: 'x).toString == "a(b * omega) & x")
    }
    it("should test failure"){
      assert(parsing.compute("---") == Seq.empty)
    }
  }

}
