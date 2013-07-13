package com.github.hexx.parsing

import scala.collection.immutable.Stack
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.{CharArrayReader, Position, Reader}

trait IndentLexical extends Lexical with IndentTokens {
  override type Elem = Char

  def token: Parser[Token]

  def whitespace: Parser[Any]

  case class IndentState(isBol: Boolean, currentIndent: Int, indentStack: Stack[Int])

  def scanner(in: String) = new IndentScanner(new CharArrayReader(in.toCharArray()), IndentState(true, 0, Stack()))

  class IndentScanner(in: Reader[Char], indentState: IndentState) extends Reader[Token] {
    private[this] def atEnd0 = in.atEnd || (whitespace(in) match { case Success(_, in0) => in0.atEnd case _ => false })

    private[this] def scan(in0: Reader[Char], indentState0: IndentState): (Token, Position, Reader[Elem], IndentState) = {
      def readWhitespace: Parser[Int] = rep(elem(' ')) ^^ (_.length)

      def skip(in1: Reader[Char]) = if (in1.atEnd) in1 else in1.rest

      def scan0(in1: Reader[Char]) =
        whitespace(in1) match {
          case Success(_, in2) =>
            if (in2.first == '\n') {
              (Terminator, in2.pos, in2.rest, indentState0.copy(isBol = true))
            } else {
              token(in2) match {
                case Success(tok, in3) =>
                  (tok, in2.pos, in3, indentState0)
                case ns: NoSuccess => (errorToken(ns.msg), ns.next.pos, skip(ns.next), indentState0)
              }
            }
          case ns: NoSuccess => (errorToken(ns.msg), ns.next.pos, skip(ns.next), indentState0)
        }

      if (indentState0.currentIndent < indentState0.indentStack.sum) {
        if (indentState0.indentStack.top <= indentState0.indentStack.sum - indentState0.currentIndent) {
          (Outdent, in0.pos, in0, indentState0.copy(isBol = false, indentStack = indentState0.indentStack.pop))
        } else {
          (errorToken("Irregular indent"), in0.pos, in0.rest, indentState0)
        }
      } else if (atEnd0 && indentState.currentIndent > 0) {
        scan(in, indentState.copy(currentIndent = 0))
      } else if (indentState0.isBol) {
        readWhitespace(in0) match {
          case Success(i, in1) =>
            if (indentState0.currentIndent < i) {
              (Indent, in1.pos, in1, IndentState(false, i, indentState0.indentStack.push(i - indentState0.currentIndent)))
            } else if (indentState0.currentIndent > i) {
              scan(in1, indentState0.copy(currentIndent = i))
            } else {
              scan(in1, indentState0.copy(isBol = false))
            }
          case ns: NoSuccess => (errorToken(ns.msg), ns.next.pos, skip(ns.next), indentState0)
        }
      } else {
        scan0(in0)
      }
    }

    private[this] lazy val (first0, pos0, rest0, indentState0) = scan(in, indentState)

    override def source: java.lang.CharSequence = in.source
    override def offset: Int = in.offset
    def first = first0
    def rest = new IndentScanner(rest0, indentState0)
    def pos = pos0
    def atEnd = atEnd0 && indentState.indentStack.isEmpty
  }
}
