/*
* Copyright 2018 Anna Vasileva
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package org.combinators.cls.ide

import org.combinators.cls.types._

import scala.util.matching.Regex
import scala.util.parsing.combinator._


class PathParser extends RegexParsers {
  val parser = new RequestParser

  val word: Regex = """[a-zA-Z0-9=>_\. \[\]]*[a-zA-Z0-9=>\.\[\]]""".r


  def ty: Parser[Type] = tyPro ~ opt("->" ~ ty) ^^ {
    case lhs ~ Some(_ ~ rhs) => Arrow(lhs, rhs)
    case lhs ~ None => lhs
  }
  def tyPro: Parser[Type] = tyProduct ~ opt("*" ~ tyProduct) ^^ {
    case lhs ~ Some(_ ~ rhs) => Product(lhs, rhs)
    case lhs ~ None => lhs
  }
  def tyProduct: Parser[Type] = tyInter ~ opt("*" ~ tyInter) ^^ {
    case lhs ~ Some(_ ~ rhs) => Product(lhs, rhs)
    case lhs ~ None => lhs
  }

  def tyInter: Parser[Type] = tyS ~ opt("&" ~ tyInter) ^^ {
    case lhs ~ Some(_ ~ rhs) => Intersection(lhs, rhs)
    case lhs ~ None => lhs
  }

  def tyS: Parser[Type] = ctor | "(" ~ ty ~ ")" ^^ { case _ ~ ty ~ _ =>  ty }


  def ctor: Parser[Type] = word ~ opt("("~ tyPro ~ ")") ^^ {
    case name ~ None => Constructor(name)
    case name ~ Some(_ ~ tys ~ _) => Constructor(name, tys)

  }

  def args: Parser[Seq[Type]] = ty ~ opt("," ~ args) ^^ {
    case lhs ~ Some(_ ~ rhs) => val slist: Seq[Type]  = Seq(lhs) ++ rhs
      slist
    case lhs ~ None => Seq(lhs)
  }

  def tyPath: Parser[Seq[Type]] = word ~ "(" ~ opt(args)~ ")" ^^ {
    case w ~ "("~ None ~ ")" => Seq()
    case w ~ "("~ Some(t) ~ ")" => t
  }

  def tgts: Parser[(Seq[Type], Type)] =  "(" ~ tyPath ~"," ~ ty ~")" ^^ {
    case  "(" ~ x ~","~ y ~")" => (x,y)
  }
}

object NewPathParser extends PathParser {
  def compute(selection: String): Option[(Seq[Type], Type)] = parseAll(tgts, selection) match {
    case Success(result, _) => Some(result)
    case _: Failure => None
  }
}


