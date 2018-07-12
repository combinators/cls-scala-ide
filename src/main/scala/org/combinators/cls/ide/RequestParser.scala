package org.combinators.cls.ide

import org.combinators.cls.types._

import scala.util.matching.Regex
import scala.util.parsing.combinator._


class RequestParser extends RegexParsers {
  val word: Regex =
    """[a-zA-Z0-9=>\. \[\]]*[a-zA-Z0-9=>\.\[\]]""".r

  def ty: Parser[Type] = tyInter ~ opt("->" ~ tyInter) ^^ {
    case lhs ~ Some(_ ~ rhs) => Arrow(lhs, rhs)
    case lhs ~ None => lhs
  }

  def tyInter: Parser[Type] = tyS ~ opt("&" ~ tyS) ^^ {
    case lhs ~ Some(_ ~ rhs) => Intersection(lhs, rhs)
    case lhs ~ None => lhs
  }

  def tyS: Parser[Type] = ctor | "(" ~ ty ~ ")" ^^ { case _ ~ ty ~ _ =>  ty }


  def ctor: Parser[Type] = word ~ opt("("~ repsep(ty, ",".r) ~ ")") ^^ {
    case name ~ None => Constructor(name)
    case name ~ Some(_ ~ tys ~ _) => Constructor(name, tys:_*)
  }

  def tgts: Parser[Seq[Type]] = rep(ty)
}

object NewRequestParser extends RequestParser {

  def compute(request: String): Seq[Type] = parseAll(tgts, request) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}