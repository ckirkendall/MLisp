package org.mlisp

import org.mlisp.utils._

abstract class Fn {
  def apply(env: Env, args: List[Exp]) : Any
}

//SPECIAL FORMS

class FnBuilder(isMacro: Boolean) extends Fn {
  def apply(origEnv: Env, exps: List[Exp]) : Fn = exps match {
    case Nil => throw new RuntimeException("invalid fn statement")
    case args::body => Helper.unwrap(args) match {
      case asyms: List[Any] => new Fn(){
	def apply(env: Env, vals: List[Exp]): Any = {
	  val nenv = new ChildEnv(origEnv)
	  if(!asyms.isEmpty){
    	    val pairs = Helper.zipArgs(Helper.listAnytoListSymbol(asyms),vals);
	    //println(pairs)
	    pairs.foreach(pair => pair._2 match {
	      case l: List[Exp] => nenv.assign(pair._1, if(isMacro) Helper.unwrap(l) else Eval.eval(l,env))
	      case x: Exp => nenv.assign(pair._1, if(isMacro) Helper.unwrap(x) else Eval.eval(x, env))
	      case _ => throw new RuntimeException("invalid arguments")
	    })
	  }
	  if(isMacro) 
	    new Thunk(List(Helper.wrap(Eval.eval(body,nenv).last)), env)
	  else
	    new Thunk(body,nenv)
	}
      } 
      case _ => throw new RuntimeException("invalid fn statement" + exps)
    }
  }
}

class DefFn extends Fn {
  def apply(env: Env, exps: List[Exp]): Any = exps match {
    case SYMBOL(sym)::body::Nil => {
      val tmp = Eval.eval(body,env)
      env.assign(sym,tmp)
      tmp
    } 
    case _ => throw new RuntimeException("invalid def statement"+exps)
  }
}

class IfFn extends Fn {
  def apply(env: Env, exps: List[Exp]): Any = exps match {
    case test::pos::neg::Nil => Eval.eval(test,env) match {
      case null => Eval.eval(neg,env)
      case false => Eval.eval(neg,env)
      case _ => Eval.eval(pos,env)
    }
    case _ => throw new RuntimeException("invalid if statement")
  }
}


class DotFn extends Fn {
  def apply(env: Env, vals: List[Exp]): Any = {
    try { 
      vals match {
	case Nil => throw new RuntimeException("invalid method call: no obj")
	case exp::SYMBOL(meth)::aexps => {
	  val obj = Eval.eval(exp,env)
	  val clazz = if(obj.isInstanceOf[java.lang.Class[_]]) obj.asInstanceOf[java.lang.Class[_]] else obj.getClass
	  val mname = meth.name
	  val fieldOption = if(aexps.isEmpty) clazz.getFields.filter(_.getName.equals(mname)).headOption else None
	  fieldOption match {
	    case Some(v) => v.get(obj)
	    case None =>{
	      val args = Eval.eval(aexps,env)
	      val rel = ClazzUtils.findBestMatchMethod(clazz, mname, args)
	      val m = rel match {
		case EXACT(m) => m
		case COMPATABLE(m) => m
		case NOREL() => throw new RuntimeException("invalid method call:"+clazz+":"+obj+":"+meth)
		
	      }
	      m.invoke(obj, args.map(_.asInstanceOf[java.lang.Object]):_*)
	    }
	  }
	}
	case _ => throw new RuntimeException("invalid method syntax:")
      }
    } catch {
      case error: Throwable => {
        println("invalid method call")
        throw error
      }
    }
  }
}


class NewFn() extends Fn {
  def apply(env: Env, vals: List[Exp]): Any = vals match {
    case SYMBOL(clazz)::expArgs => {
      expArgs match { 
	case Nil => Class.forName(clazz.name).newInstance
	case _ => { 
          val args = Eval.eval(expArgs,env)
          val rel = ClazzUtils.findBestMatchConstructor(Class.forName(clazz.name),args)
          val m = rel match {
            case EXACT(m) => m
            case COMPATABLE(m) => m
            case NOREL() => throw new RuntimeException("invalid method call")
          }
          m.newInstance(args.map(_.asInstanceOf[java.lang.Object]):_*)
	}
      }
    }
    case _ => throw new RuntimeException("invalid constructor")
  }
}


class TryFn extends Fn {
  def isCatch(exp: Exp) = exp match {
    case LIST(v) => v match {
      case xs::tail => xs match {
        case SYMBOL(Symbol("catch")) => true
        case _ => false
      }
      case _ => false
    }
  }
  def catchTransform(cExp: LIST): (Class[_], Exp) = cExp.value match {
    case _::arg::body => arg match {
      case LIST(SYMBOL(x)::SYMBOL(y)::Nil) => (Class.forName(y.name),LIST(List('let,List(x,Symbol("*ex*"))).map(Helper.wrap):::body))
      case _ => throw new RuntimeException("invalid argument to catch statement")
    }
    case _ => throw new RuntimeException("invalid catch statement")
  }
  
  def apply(env: Env, vals: List[Exp]): Any = {
    val body = vals.filter((a) => !isCatch(a))
    val catches = vals.filter((a) => isCatch(a)).map((a) => catchTransform(a.asInstanceOf[LIST]))
    try{ 
      Eval.eval(body,env).last
    }catch {
      case error: java.lang.Throwable => catches.dropWhile((exp) => !exp._1.isAssignableFrom(error.getClass())) match {
        case Nil => throw error
        case head::tail => {
          env.assign(Symbol("*ex*"), error)
          Eval.eval(head._2,env,false)
          error.printStackTrace()
          "Error"
        }
      }
    }
  }
}


class Thunk(body: List[Exp], env: Env){
  def apply(): Any = {
    Eval.evalWithTailCall(body, env).last
  }
}
