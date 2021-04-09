/*
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

package org.combinators.cls.ide.parser

import org.combinators.cls.ide.filter.{ApplicativePattern, ApplyPattern, CombinatorPattern, StarPattern}

import scala.util.matching.Regex
import scala.util.parsing.combinator._


class FilterApplyParser extends RegexParsers {
  val word: Regex =
    """[a-zA-Z0-9=>\. \[\]]*[a-zA-Z0-9=>\.\[\]]""".r

  def star: Parser[ApplicativePattern] = "*" ^^ {
    case _ => StarPattern()
  }
  def tyProduct: Parser[Seq[ApplicativePattern]] = muster ~ opt("," ~ muster) ^^ {
    case lhs ~ Some(_ ~ rhs) => Seq(lhs, rhs)
    case lhs ~ None => Seq(lhs)
  }
  def muster: Parser[ApplicativePattern] = combinator | applyP | star

  def combinator: Parser[ApplicativePattern] = word ~ opt("("~ tyProduct ~ ")") ^^ {
    case name ~ None => CombinatorPattern(name)

  }
  def applyP: Parser[ApplicativePattern] = word
}

object NewFilterParser extends FilterParser {

  def compute(request: String): Option[ApplicativePattern] = parseAll(muster, request) match {
    case Success(result, _) => Some(result)
    case failure: NoSuccess => None
  }
}
*/
