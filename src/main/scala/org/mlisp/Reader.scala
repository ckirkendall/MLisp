package org.mlisp 

import scala.util.parsing.combinator.RegexParsers

class Reader extends RegexParsers {
  def sym: Parser[SYMBOL] = """[a-zA-Z$+\-?\*][a-zA-Z0-9$\*\-?\.]*""".r ^^ { s => SYMBOL(Symbol(s)) }
  def num: Parser[INT] = """\d+""".r ^^ { i => INT(new java.lang.Long(i)) }
  def str: Parser[STRING] = "\"" ~> """([^""\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ {  str => STRING(str) } //"
  def lst: Parser[LIST] = "(" ~> repsep(exp, """\s*""".r) <~ ")" ^^ { args => LIST(args) }
  def quote: Parser[QUOTE] = "'" ~> act ^^ { obj => QUOTE(obj) }
  def unquote: Parser[UNQUOTE] = "~" ~> act ^^ { obj => UNQUOTE(obj) }
  def amp: Parser[SYMBOL] = "&" ^^ { s => SYMBOL(Symbol(s)) }
  def dot: Parser[SYMBOL] = "." ^^ { s => SYMBOL(Symbol(s)) }

  def act = sym | lst
  def exp = str | num | lst | sym | quote | unquote | amp | dot
  def explist = rep(exp)
}
