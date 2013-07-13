package com.github.hexx.parsing

import scala.util.parsing.combinator.syntactical.TokenParsers

trait IndentTokenParsers extends TokenParsers {
  type Tokens <: IndentTokens
  import lexical._

  def indent: Parser[String] = elem("indent", _ == Indent) ^^ (_.chars)

  def outdent: Parser[String] = elem("outdent", _ == Outdent) ^^ (_.chars)

  def terminator: Parser[String] = elem("terminator", _ == Terminator) ^^ (_.chars)
}
