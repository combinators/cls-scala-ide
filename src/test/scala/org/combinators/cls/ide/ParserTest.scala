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
      assert(parsing.compute(('a('b, 'c)).toString()) == Seq(Constructor("a", Constructor("b"), Constructor("c"))))
    }
    it("should parse constructors combined with arrows") {
      assert(parsing.compute(('a =>: 'b).toString()) == Seq(Arrow(a, b)))
      assert(parsing.compute(('a('b) =>: 'c).toString()) == Seq(Arrow(Constructor("a", b), c)))
      assert(parsing.compute(('a =>: 'b('a, 'c)).toString()) == Seq(Arrow(a, Constructor("b", a, c))))
    }
    it("should parse constructors combined with intersections") {
      assert(parsing.compute(('a :&: 'b).toString()) == Seq(Intersection(a, b)))
      assert(parsing.compute(('a('b) :&: 'c).toString()) == Seq(Intersection(Constructor("a", b), c)))
      assert(parsing.compute(('a :&: 'b('a, 'c)).toString()) == Seq(Intersection(a, Constructor("b", a, c))))
    }
    it("should pretty print almost identically") {
      assert(('a('b, Omega) :&: 'x).toString == "a(b, omega) & x")
    }
    it("should test failure"){
      assert(parsing.compute("---") == Seq.empty)
    }
  }

}
