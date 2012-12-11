package org.mlisp

import org.mlisp.utils._

object Main extends App {
    override def main(args: Array[String]) {
      val reader = new Reader()
  
      val env = new ChildEnv(new NilEnv)
      env.assign(Symbol("if"), new IfFn())
      env.assign(Symbol("def"), new DefFn())
      env.assign(Symbol("fn"), new FnBuilder(false))
      env.assign(Symbol("macro"), new FnBuilder(true))
      env.assign(Symbol("."), new DotFn())
      env.assign(Symbol("new"), new NewFn())
      env.assign(Symbol("try"), new TryFn())
      env.assign(Symbol("*reader*"), reader)
      env.assign(Symbol("*coreenv*"), env)  
       
      val coreSource = io.Source.fromInputStream(getClass.getResourceAsStream("/org/mlisp/core.lisp"))
      val coreLib = coreSource.mkString
      coreSource.close
      val replSource = io.Source.fromInputStream(getClass.getResourceAsStream("/org/mlisp/repl.lisp"))
      val replLib = replSource.mkString
      replSource.close


      val coreExp = reader.parseAll(reader.explist, coreLib)
      coreExp.get.map(Eval.eval(_,env))
      val testExp = reader.parseAll(reader.explist, replLib)
      testExp.get.map(Eval.eval(_,env))
    } 
}
