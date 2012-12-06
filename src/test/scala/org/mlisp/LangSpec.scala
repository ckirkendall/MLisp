package org.mlisp

import org.scalatest.FunSpec
import java.lang.Long

class LangSpec extends FunSpec {
  val reader=new Reader() 
  
  def setupEnv() : Env = {
    val env = new ChildEnv(new NilEnv)
    env.assign(Symbol("true"), java.lang.Boolean.TRUE)
    env.assign(Symbol("false"), java.lang.Boolean.FALSE)
    env.assign(Symbol("if"), new IfFn())
    env.assign(Symbol("def"), new DefFn())
    env.assign(Symbol("fn"), new FnBuilder(false))
    env.assign(Symbol("macro"), new FnBuilder(true))
    env.assign(Symbol("."), new DotFn())
    env.assign(Symbol("*reader*"), reader)
    env.assign(Symbol("*coreenv*"), env)
    env
  }

  describe("An INT - 1,2,3") {
    val str="1"

    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,str)
      expectResult(INT(new Long("1"))) { exp.get }
    }

    it("should resolve to Long") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str)
      expectResult(new Long("1")) { Eval.eval(exp.get, env) }
    }
  }

  describe("A STRING - \"test\"") {
    val str = "\"test\""

    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,str)
      expectResult(STRING("test")) { exp.get }
    }

    it("should resolve to a string") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str)
      expectResult("test") { Eval.eval(exp.get, env) }
    }
  }

  describe("A SYMBOL - a,b,c") {
    val symstr="a"

    it("should be parseable") {
      val symExp = reader.parseAll(reader.exp,symstr)
      expectResult(SYMBOL('a)) { symExp.get }
    }

    it("should resolve to a lookup in env"){
      val env = setupEnv()
      env.assign(Symbol("a"), "test")
      val symExp = reader.parseAll(reader.exp,symstr)
      expectResult("test") { Eval.eval(symExp.get, env) }
    }

  }

  describe("A LIST - (a 1 2)"){
    val lstr="(a 1 2)"

    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult(LIST(List(SYMBOL(Symbol("a")),INT(new Long("1")),INT(new Long("2"))))) { exp.get }
    }

    it("should call a function in the environment"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" })
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult("test") { Eval.eval(exp.get, env) }
    }

  }


  describe("A QUOTED List - '(a 1 2)"){
    val lstr="'(a 1 2)"

    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult(QUOTE(LIST(List(SYMBOL(Symbol("a")),INT(new Long("1")),INT(new Long("2")))))) { exp.get }
    }

    it("should return a list and not call a function"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" })
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult(List('a,new Long("1"),new Long("2"))) { Eval.eval(exp.get, env) }
    }
  }

  describe("A QUOTED Symbol - 'a,'b,'c"){
    val lstr="'test"

    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult(QUOTE(SYMBOL(Symbol("test")))) { exp.get }
    }

    it("should return a symbol and not look up in the environment"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" })
      val exp = reader.parseAll(reader.exp,lstr)
      expectResult('test) { Eval.eval(exp.get, env) }
    }

  }

  describe("An UNQUOTED List - ~(a b c)"){
    val ustrInvalid=("~(a 1 2)")
    val ustr=("'(b ~(a 'b 'c) d)")
    
    it("should be parseable") {
      val exp = reader.parseAll(reader.exp,ustrInvalid)
      expectResult(UNQUOTE(LIST(List(SYMBOL('a),INT(new Long("1")),INT(new Long("2")))))) { exp.get }
    }

    it("should evaluate the expression when inside a quoted list"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" })
      val exp = reader.parseAll(reader.exp,ustr)
      expectResult(List('b,"test",'d)) { Eval.eval(exp.get, env) }
    }

    it("should error out if present outside of a quoted list"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" })
      val exp = reader.parseAll(reader.exp,ustrInvalid)
      intercept[RuntimeException] { Eval.eval(exp.get, env) }
    }
  }

  describe("A 'def' expression - (def a 1)") {
    val defstr="(def a 1)"

    it("should be parsable") {
      val defExp = reader.parseAll(reader.exp,defstr)
    }

    it("should evaluate to the value of the expression") {
      val env = setupEnv()
      val defExp = reader.parseAll(reader.exp,defstr) 
      expectResult(new Long("1")) { Eval.eval(defExp.get, env) }  
    }

    it("should throw an exception if we have invalid format") {
      val env = setupEnv()
      val defExp = reader.parseAll(reader.exp,"(def a 1 2)") 
      intercept[RuntimeException] { Eval.eval(defExp.get, env) }
    }

    it("should assign the symbol to the result") {
      val env = setupEnv()
      val defExp = reader.parseAll(reader.explist, defstr + " a")
      expectResult(new Long("1")) { Eval.eval(defExp.get, env) }  
    } 
  }

  describe("An 'fn' expression - (fn (x) x)") {
    val fnstr="((fn (x) x) 1)"

    it("should be parsable") {
      val fnExp = reader.parseAll(reader.exp,fnstr)
    }

    it("should evaluate the body when called") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"((fn () 1))") 
      expectResult(new Long("1")) { Eval.eval(fnExp.get, env) }  
    }

    it("should take parameters and evaluate the body with these parameters bound in context when called") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"((fn (x) x) 1)") 
      expectResult(new Long("1")) { Eval.eval(fnExp.get, env) }  
    }

    it("should allow '&' to be used capture vargs as a list"){
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"((fn (x & y) y) 'a 'b 'c)") 
      expectResult(List('b,'c)) { Eval.eval(fnExp.get, env) }  
    }

    it("should throw an exception if we have invalid format") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"(fn 1 2)") 
      intercept[RuntimeException] { Eval.eval(fnExp.get, env) }
    }
  }
  
  describe("An 'macro' expression - (macro (x) '(tmp ~x))") {
    val mstr="((macro (x) x) (a))"

    it("should be parsable") {
      val fnExp = reader.parseAll(reader.exp,mstr)
    }

    it("should pass the args in unevaluated, evaluate body in context and evaluate result in parent context") {
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" } )
      val fnExp = reader.parseAll(reader.exp,mstr) 
      expectResult("test") { Eval.eval(fnExp.get, env) }  
    }


    it("should allow '&' to be used capture vargs as a list"){
      val env = setupEnv()
      env.assign(Symbol("a"), new Fn { def apply(env: Env, exps: List[Exp]) : Any = "test" } )
      val exp = reader.parseAll(reader.exp,"((macro (x & y) y) b a 1)") 
      expectResult("test") { Eval.eval(exp.get, env) }  
    }

    it("should throw an exception if we have invalid format") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"(macro 1 2)") 
      intercept[RuntimeException] { Eval.eval(fnExp.get, env) }
    }
  }

  describe("An 'if' expression - (if test fist second)") {
    val tstr="(if true true false)"
    val fstr="(if false true false)"
    val ostr="(if 'a true false)"

    it("should be parsable") {
      val fExp = reader.parseAll(reader.exp,tstr)
      val fExp2 = reader.parseAll(reader.exp,fstr)
    }

    it("should evaluate first expression if test return true") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,tstr) 
      expectResult(true) { Eval.eval(exp.get, env) }  
    }

    it("should evaluate second expression if test return false") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,fstr) 
      expectResult(false) { Eval.eval(exp.get, env) }  
    }

    it("should evalute first expression if test return non null obj") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,tstr) 
      expectResult(true) { Eval.eval(exp.get, env) }  
    }

    it("should throw an exception if we have invalid format") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"(if 1 2)") 
      intercept[RuntimeException] { Eval.eval(fnExp.get, env) }
    }
  }

  describe("A '.' expression - (. (. java.lang.System out) println 'a)") {
    val str="(. \"test\" length)"
    val str2="(. \"test\" length)"
    val str3="(. java.lang.System out)"
    val str4="(. '(1 2 3) size)"

    it("should be parsable") {
      val fExp = reader.parseAll(reader.exp,str)
    }

    it("should access a field of an object if it exists and no args are passed") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str) 
      expectResult(4) { Eval.eval(exp.get, env) }  
    }

    it("should call the method if it exist on an object") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str2) 
      expectResult(4) { Eval.eval(exp.get, env) }  
    }

    it("should access a field of class if it exists and no args are passed") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str3) 
      expectResult(System.out) { Eval.eval(exp.get, env) }  
    }

    it("should call the static method if it exist on an class") {
      val env = setupEnv()
      val exp = reader.parseAll(reader.exp,str4) 
      expectResult(List(1,2,3).size) { Eval.eval(exp.get, env) }  
    }

    it("should throw an exception if we have invalid format") {
      val env = setupEnv()
      val fnExp = reader.parseAll(reader.exp,"(if 1 2)") 
      intercept[RuntimeException] { Eval.eval(fnExp.get, env) }
    }
  }
}
