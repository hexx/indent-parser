package com.github.hexx.parsing

import scala.util.parsing.combinator.token.Tokens

trait IndentTokens extends Tokens {
  case object Indent extends Token {
    def chars = "INDENT"
  }

  case object Outdent extends Token {
    def chars = "OUTDENT"
  }

  case object Terminator extends Token {
    def chars = "TERMINATOR"
  }
}
