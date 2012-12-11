package org.mlisp
 
abstract class Exp

case class INT(value: java.lang.Long) extends Exp
case class STRING(value: String) extends Exp
case class SYMBOL(value: Symbol) extends Exp
case class LIST(value: List[Exp]) extends Exp
case class QUOTE(value: Exp) extends Exp
case class UNQUOTE(value: Exp) extends Exp
