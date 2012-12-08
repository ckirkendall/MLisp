package org.mlisp.utils

import org.mlisp._
import java.math.BigInteger

object Helper {
  
  def wrap(obj: Any): Exp = obj match {
    case exp: List[Any] => exp match {
      case Nil => LIST(Nil)
      case xs::tail => xs match {
        case Symbol("~") => UNQUOTE(wrap(tail.head)) //TODO should use head option
        case Symbol("'") => QUOTE(wrap(tail.head)) //TODO should use head option
        case _ => LIST(exp.map(wrap))
      }
    }
    case exp: java.lang.Long => INT(exp)
    case exp: String => STRING(exp)
    case exp: Symbol => SYMBOL(exp)
    case _ => 
      println(obj)
      throw new RuntimeException("invalid wrap argument")
  }
  
  
  def unwrap(exp: Exp): Any = {
    unwrap(exp,false, new NilEnv)
  }
  
  def unwrap(list: List[Exp]): Any = {
    list.map(unwrap)
  }
  
  def unwrap(exp: Exp, unquote: Boolean, env: Env) : Any = {
    exp match {
	    case INT(v) => v
	    case STRING(v) => v
	    case SYMBOL(v) => v
	    case UNQUOTE(v) => if(unquote) Eval.eval(v,env,true) else Symbol("~")::List(unwrap(v))
	    case QUOTE(v) => Symbol("'")::List(unwrap(v,unquote,env))
	    case LIST(v) => v.map(a => unwrap(a, unquote,env)).toList
	  }
    }
  
  def listAnytoListSymbol(args: List[Any]): List[Symbol] = args match {
    case Nil => Nil
    case xs::tail => xs match {
      case arg: Symbol => List(arg):::listAnytoListSymbol(tail)
      case _ => throw new RuntimeException("invalid fn statement")
    }
  }
  
  def buildAssignmentList(as: List[Exp]) : List[(Symbol, Exp)] = {
    val pairs = as.grouped(2);
    pairs.map { 
      _ match {
        case Nil => throw new RuntimeException("invalid assigment")
        case xs::tail => Helper.unwrap(xs) match {
          case xs: Symbol => (xs, tail.head)
          case _ => throw new RuntimeException("invalid assignment list")
        }
      }
    }.toList
  }

  def zipArgs(syms: List[Symbol], vals: List[Exp]): List[(Symbol, Any)] = {
    (syms, vals) match {
      case (Nil,Nil) => Nil
      case (Nil, v::vtail) => throw new RuntimeException("invalid number of args")
      case (s::stail,Nil) => throw new RuntimeException("invalid number of args")
      case (s::stail,v::vtail) => s match {
        case '& => (stail.head,v::vtail)::Nil
        case _ => (s,v)::zipArgs(stail,vtail)
      }
    }
  }
  
}
