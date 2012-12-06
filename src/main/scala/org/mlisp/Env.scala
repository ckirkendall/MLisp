package org.mlisp

import scala.collection.mutable.HashMap

abstract class Env {
  def assign(sym: Symbol, ex: Any): Unit
  def lookup(sym: Symbol): Any
}

class ChildEnv(parent: Env) extends Env {
  val hash = HashMap[Symbol, Any]()
  hash.put(Symbol("*curenv*"),this)
  
  def assign(sym: Symbol, ex: Any) = {
    hash.put(sym, ex)
  }
  
  def lookup(sym: Symbol) : Any = {
    try{
      if(!sym.name.equals(".") && sym.name.contains("."))
	Class.forName(sym.name)
      else
	hash.get(sym).getOrElse(parent.lookup(sym))
    }catch {
      case _ => println("error looking up symbol:" + sym.name)
    }
  }
}

class NilEnv extends Env {
  def assign(sym: Symbol, ex: Any): Unit = Unit
  def lookup(sym: Symbol): Any = throw new RuntimeException("invalid reference:"+sym)
}
