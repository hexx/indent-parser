package com.github.hexx.yaml

import com.github.hexx.parsing.{IndentTokens, IndentLexical, IndentTokenParsers}
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.Reader

trait YamlTokens extends IndentTokens {
  case class Keyword(chars: String) extends Token {
    override def toString = s"""`${chars}'""""
  }

  case class NumericLit(chars: String) extends Token {
    override def toString = chars
  }

  case class StringLit(chars: String) extends Token {
    override def toString = s""""${chars}""""
  }
}

class YamlLexical extends IndentLexical with YamlTokens {
  override def token: Parser[Token] = {
    val reserved = List("true", "false", "null")

    val delimiters = List(":", "-")

    def string = '\"' ~> rep(chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ (_.mkString)

    def number = digit ~ rep( digit ) ^^ { case first ~ rest => first :: rest mkString "" }

    def delim: Parser[Token] = delimiters.map(d => accept(d.toList) ^^ (_ => Keyword(d))).reduce(_ | _) | failure("no matching delimiter")

    def keywordOrString = rep1(letter) ^^ { cs =>
      val k = cs.mkString
      if (reserved contains k) {
        Keyword(k)
      } else {
        StringLit(k)
      }
    }

    ( string ^^ StringLit
    | number ^^ NumericLit
    | delim
    | keywordOrString
    | failure("illegal character")
    )
  }

  override def whitespace = rep(elem("space char", ch => ch <= ' ' && ch != EofCh && ch != '\n'))
}

sealed trait Yaml
case class YMap(map: Map[Yaml, Yaml]) extends Yaml
case class YSeq(seq: Seq[Yaml]) extends Yaml
case class YNumber(number: Double) extends Yaml
case class YString(scalar: String) extends Yaml
case object YTrue extends Yaml
case object YFalse extends Yaml
case object YNull extends Yaml

object YamlParsers extends IndentTokenParsers {
  type Tokens = YamlTokens
  val lexical = new YamlLexical

  def parse(s: String) = {
    import lexical.{Keyword, NumericLit, StringLit}

    implicit def stringToKeywordParser(chars: String): Parser[String] = accept(Keyword(chars)) ^^ (_.chars)

    def yaml    : Parser[Yaml] = map | seq | scalar | indent ~> yaml <~ outdent | terminator ~> yaml
    def scalar  : Parser[Yaml] = (num | str | ytrue | yfalse | ynull) <~ rep1(terminator)
    def num     : Parser[Yaml] = elem("number", _.isInstanceOf[NumericLit]) ^^ (n => YNumber(n.chars.toDouble))
    def str     : Parser[Yaml] = elem("string", _.isInstanceOf[StringLit])  ^^ (s => YString(s.chars))
    def ytrue   : Parser[Yaml] = "true"  ^^^ YTrue
    def yfalse  : Parser[Yaml] = "false" ^^^ YFalse
    def ynull   : Parser[Yaml] = "null"  ^^^ YNull
    def map     : Parser[Yaml] = rep1(mapEntry) ^^ (es => YMap(es.toMap))
    def seq     : Parser[Yaml] = rep1("-" ~> yaml) ^^ YSeq

    def mapEntry: Parser[(Yaml, Yaml)] = str ~ ":" ~ yaml ^^ { case key ~ _ ~ value => (key, value) }

    phrase(yaml)(lexical.scanner(s))
  }

  def readerToList[T](r: Reader[T]): List[T] =
    if (r.atEnd) {
      List()
    } else if (r.first.isInstanceOf[lexical.ErrorToken]) {
      println(r.first)
      List()
    } else {
      r.first :: readerToList(r.rest)
    }
}
