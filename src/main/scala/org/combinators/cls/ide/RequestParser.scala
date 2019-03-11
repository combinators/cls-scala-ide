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


class RequestParser extends RegexParsers {
  val word: Regex =
    """[a-zA-Z0-9=>\. \[\]]*[a-zA-Z0-9=>\.\[\]]""".r

  def ty: Parser[Type] = tyPro ~ opt("->" ~ ty) ^^ {
    case lhs ~ Some(_ ~ rhs) => Arrow(lhs, rhs)
    case lhs ~ None => lhs
  }
  def tyPro: Parser[Type] = tyProduct ~ opt("*" ~ tyPro) ^^ {
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
  def tgts: Parser[Seq[Type]] = rep(ty)
}

object NewRequestParser extends RequestParser {

  def compute(request: String): Seq[Type] = parseAll(tgts, request) match {
    case Success(result, _) => result
    case failure: NoSuccess => Seq.empty
  }
}