package org.mlisp

import org.mlisp.utils._
import scala.util.parsing.combinator.RegexParsers

//Defining the Expression Tree (AST)
abstract class Exp

case class INT(value: java.lang.Long) extends Exp
case class STRING(value: String) extends Exp
case class SYMBOL(value: Symbol) extends Exp
case class LIST(value: List[Exp]) extends Exp
case class QUOTE(value: Exp) extends Exp
case class UNQUOTE(value: Exp) extends Exp


//Defining the Reader
class Reader extends RegexParsers {
  def exp : Parser[Exp] = "" ^^ { s => new Exp(){} }
  def explist = rep(exp)
}

//Defining Eval
object Eval {  
  def eval(exp: Exp, env: Env) : Any = None

  def eval(lst: List[Exp], env: Env) : Any = None
}

//Closure Structure
abstract class Fn {
  def apply(env: Env, args: List[Exp]) : Any
}


object Main extends App {
    override def main(args: Array[String]) {
      val env = new ChildEnv(new NilEnv)
      val reader=new Reader()
      val coreSource = io.Source.fromFile("/org/mlisp/core.lisp")
      val coreLib = coreSource.mkString
      coreSource.close
      val testStr = """
	(println "I work")
      """
      val coreExp = reader.parseAll(reader.explist, coreLib)
      coreExp.get.map(Eval.eval(_,env))
      val testExp = reader.parseAll(reader.explist, testStr)
      testExp.get.map(Eval.eval(_,env))
    } 
}
