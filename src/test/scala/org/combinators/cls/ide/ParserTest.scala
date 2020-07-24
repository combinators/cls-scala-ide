package org.combinators.cls.ide


import org.combinators.cls.ide.filter.{Star, Term}
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.scalatest.FunSpec

import scala.util.parsing.combinator.Parsers



class ParserTest extends FunSpec {

  val a = Constructor("a")
  val b = Constructor("b")
  val c = Constructor("c")
  val d = Constructor("d")
  val e = Constructor("e")
  val f = Constructor("f")
  val parsing = NewRequestParser
  val parsingFilter = NewFilterParser

  describe("Muster Parser"){
    it("should parse terms"){
      assert(parsingFilter.compute("a") == Some(Term("a", Seq.empty)))
      assert(parsingFilter.compute("a(b)") == Some(Term("a", Seq(Term("b", Seq.empty)))))
    }
    it("should parse star"){
      assert(parsingFilter.compute("*") == Some(Star()))
      assert(parsingFilter.compute("a(*)") == Some(Term("a", Seq(Star()))))
      assert(parsingFilter.compute("a(*, *)") == Some(Term("a", Seq(Star(), Star()))))
      assert(parsingFilter.compute("a(b(*))") == Some(Term("a", Seq(Term("b", Seq(Star()))))))

    }
  }

  describe("Constructor Parser") {
    it("should parse combinators") {
      assert(parsing.compute("a") == Seq(a))
    }
    it("should parse parentheses") {
      assert(parsing.compute("(a)") == Seq(a))
    }
    it("should parse constructor with arguments") {
      assert(parsing.compute(('a ('b, 'c)).toString()) == Seq(Constructor("a", Product(b, c))))
      assert(parsing.compute(('a ('b, 'c, 'd)).toString()) == Seq(Constructor("a",Product(Product(Constructor("b"), Constructor("c")), Constructor("d")))))
   }
    it("should parse precedence over products to the left ") {
      assert(parsing.compute(('a <*> 'b :&: 'c).toString()) == Seq(Product(a, Intersection(b, c))))
    }
    it("should take precedence over products to the right ") {
      assert(parsing.compute(('a :&: 'b <*> 'c).toString()) == Seq(Product(Intersection(a, b), c)))
    }

    it("should be right associative") {
      assert((a :&: b :&: c) == Intersection(a, Intersection(b, c)))
      assert(parsing.compute((a =>: b =>: c).toString()) == Seq(Arrow(a, Arrow(b, c))))
    }
    it("should be left associative") {
      assert(parsing.compute((a <*> b <*> c).toString()) == Seq(Product(Product(a, b), c)))
    }

    it("should parse constructors combined with arrows") {
      assert(parsing.compute((a =>: b =>: c).toString()) == Seq(Arrow(a, Arrow(b, c))))
      assert(parsing.compute(('a =>: 'b).toString()) == Seq(Arrow(a, b)))
      assert(parsing.compute(('a('b) =>: 'c).toString()) == Seq(Arrow(Constructor("a", b), c)))
      assert(parsing.compute(('a =>: 'b('a, 'c)).toString()) == Seq(Arrow(a, Constructor("b", Product(a, c)))))
      assert((a =>: b <*> c) == Arrow(a, Product(b, c)))
      assert((a <*> b =>: c) == Arrow(Product(a, b), c))
    }
    it("should parse constructors combined with intersections") {
      assert(parsing.compute(('a :&: 'b).toString()) == Seq(Intersection(a, b)))
      assert(parsing.compute(('a :&: 'b :&: 'c).toString()) == Seq(Intersection(a, Intersection(b, c))))
      assert(parsing.compute(('a :&: 'b :&: 'c :&: 'd).toString()) == Seq(Intersection(a, Intersection(b, Intersection(c, d)))))
      assert(parsing.compute(('a('b) :&: 'c).toString()) == Seq(Intersection(Constructor("a", b), c)))
      assert(parsing.compute(('a :&: 'b('a, 'c)).toString()) == Seq(Intersection(a, Constructor("b", Product(a, c)))))
    }

    it("should pretty print almost identically") {
     // assert(parsing.compute(('a('b, Omega) :&: 'c).toString) == Seq(Intersection(Constructor("a", Product(b, Omega)), c)))
    }
    it("should test failure"){
      assert(parsing.compute("---") == Seq.empty)
    }
  }
  describe("Intersection notations") {
    it("should preserve order") {
      assert(parsing.compute((a :&: b).toString()) == Seq(Intersection(a, b)))
    }
    it("should be right associative") {
      assert(parsing.compute((a :&: b :&: c).toString()) == Seq(Intersection(a, Intersection(b, c))))
    }
    it("should take precedence over arrow sources") {
      assert(parsing.compute((a :&: b =>: c).toString()) == Seq(Arrow(Intersection(a, b), c)))
    }
    it("should take precedence over arrow targets") {
      assert(parsing.compute((a =>: b :&: c).toString()) == Seq(Arrow(a, Intersection(b, c))))
    }
    it("should take precedence over products to the right ") {
      assert(parsing.compute((a :&: b <*> c).toString()) == Seq(Product(Intersection(a, b), c)))
    }
    it("should take precedence over products to the left ") {
      assert(parsing.compute((a <*> b :&: c).toString()) == Seq(Product(a, Intersection(b, c))))
    }
    it("should pretty print almost identically") {
      assert(
        ((a :&: b) :&: c :&: (a :&: c =>: b) :&: (a =>: c :&: b) :&: (a :&: b <*> c) :&: (a <*> b :&: c)).toString ==
          "a & b & c & (a & c -> b) & (a -> c & b) & (a & b * c) & (a * b & c)"
      )
    }
      it("should be identically") {
        assert(parsing.compute(((a :&: b) :&: c :&: (a :&: c =>: b) :&: (a =>: c :&: b) :&: (a :&: b <*> c) :&: (a <*> b :&: c)).toString) ==
            Seq(Intersection(a, Intersection(b, Intersection(c, Intersection(Arrow(Intersection(a, c), b), Intersection(Arrow(a, Intersection(c, b)), Intersection(Product(Intersection(a, b), c), Product(a, Intersection(b, c))))))))))

      }
  }

}
