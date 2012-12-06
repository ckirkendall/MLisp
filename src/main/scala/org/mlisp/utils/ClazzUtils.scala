package org.mlisp.utils
import java.lang.reflect._
import java.lang.Class
import scala.collection.mutable.HashMap
import scala.collection.immutable.Queue

abstract class Relation[T]
case class EXACT[T](obj: T) extends Relation[T]
case class COMPATABLE[T](obj: T) extends Relation[T]
case class NOREL[T] extends Relation[T]

object ClazzUtils {
  val topClass = (new Object).getClass()
  val methodCache = new HashMap[String, Relation[_]]();
  
  def createCacheKey(clazz: Class[_], methodName: String, args: List[Any]): String = {
    val params = args.foldLeft("")((a, b)=> a + (if(b == null) "null" else b.getClass.getName))
    clazz.getName+methodName+params
  }
  
  def addCache[T](key: String, rel: Relation[T]): Relation[T] = {
    methodCache+=((key,rel))
    rel
  }
  
  def findBestMatchMethod(clazz: Class[_], methodName: String, args: List[Any]): Relation[Method] = {
	val cacheKey=createCacheKey(clazz, methodName, args)
	val cached = methodCache.get(cacheKey)
	cached match {
	  case Some(v) => v.asInstanceOf[Relation[Method]] 
	  case None => 
	    val methods=clazz.getDeclaredMethods().filter(_.getName.equals(methodName))
	    val compat=methods.foldLeft(Queue.empty[Relation[Method]])((a, b) => a + getRelation(b, args)).filter(!_.equals(NOREL()))
	    val exacts = compat.filter(_ match { case EXACT(m) => true; case _ => false })
	    exacts.headOption match {
	      case Some(e) => addCache(cacheKey,e)
	      case None => 
	        val parentRelation = if(clazz.equals(topClass)) NOREL[Method]() else findBestMatchMethod(clazz.getSuperclass(), methodName, args) 
	        parentRelation match {
	          case EXACT(p) => parentRelation
	          case _ => if(compat.isEmpty) parentRelation else compat.head
	        }
	    }
	}
	
  }
  
  def findBestMatchConstructor[T](clazz: Class[T], args: List[Any]) : Relation[Constructor[T]] ={
    val cacheKey=createCacheKey(clazz, "__C", args)
    val cached = methodCache.get(cacheKey)
    cached match {
	  case Some(v) => v.asInstanceOf[Relation[Constructor[T]]] 
	  case None => 
	    val cons=clazz.getDeclaredConstructors()
	    val compat=cons.foldLeft(Queue.empty[Relation[Constructor[T]]])((a, b) => a + getRelation(b.asInstanceOf[Constructor[T]], args)).filter(!_.equals(NOREL()))
	    val exacts = compat.filter(_ match { case EXACT(m) => true; case _ => false })
	    exacts.headOption match {
	      case Some(e) => addCache(cacheKey,e)
	      case None => compat.head
	    }
	}
  }
  
  
  def testClass[T](c1: Class[_], c2: Class[_], obj: T): Relation[T] = {
    if(c2.equals(c1))  EXACT(obj)
    else if(c1==null && !c2.isPrimitive()) COMPATABLE(obj)
    else if (c2.isAssignableFrom(c1)) COMPATABLE(obj)
    else NOREL()
  }
  
  def getRelation[T](cons: T{def getParameterTypes() : scala.Array[Class[_]]}, args: List[Any]): Relation[T] = {
    if(cons.getParameterTypes().length == args.size){
      val zipArgs = args.zip(cons.getParameterTypes())
      val rels = zipArgs.map((a) => testClass((if (a._1==null) null else a._1.getClass), a._2, cons))
      rels.filter(_.equals(NOREL())).headOption match {
        case Some(n) => n.asInstanceOf[Relation[T]]
        case None => rels.filter(_.equals(COMPATABLE(cons))).headOption match {
          case Some(c) => c.asInstanceOf[Relation[T]]
          case None => EXACT(cons)
        }
      }
    }else{
      NOREL()
    }
  }
  
 
}
