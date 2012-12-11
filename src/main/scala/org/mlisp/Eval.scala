package org.mlisp
 
import org.mlisp.utils._

object Eval { 
  def eval(exp: Exp, env: Env) : Any = eval(exp,env,false)

  def eval(exp: Exp, env: Env, tailCall: Boolean) : Any = {
    exp match { 
      case INT(i) => i
      case STRING(str) => str
      case SYMBOL(sym) => env.lookup(sym)
      case QUOTE(exp) => Helper.unwrap(exp,true,env)
      case UNQUOTE(exp) => throw new RuntimeException("naked unquote")
      case LIST(lst) => lst match {
	case fexp::args => eval(fexp, env) match {
	  case fn: Fn => {
	    val fval=fn(env,args)
	    if(tailCall) fval
	    else{
	      fval match {
		case th: Thunk => eval(th)
		case _ => fval
	      }
	    }
	  }
	  case _ => throw new RuntimeException("invalid function call")
	}
	case Nil => Nil
      }
    }
  } 

  def eval(lst: List[Exp], env: Env) : List[Any] = { 
    lst.map(eval(_,env,false))
  }

  def eval(thunk: Thunk): Any = {
    val tmp=thunk()
    tmp match {
      case th: Thunk => eval(th)
      case _ => tmp
    }
  }
 
  def evalWithTailCall(list: List[Exp], env: Env): List[Any] = list match {
    case Nil => Nil
    case xs::Nil => List(eval(xs,env,true))
    case xs::tail => eval(xs,env,false)::evalWithTailCall(tail,env)
  }
}
