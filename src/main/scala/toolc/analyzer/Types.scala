package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    def getType: Type
  }
  
  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean = tpe == this
  }
  
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }
  
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }
  
  case object TInt extends Type {
    override def toString = "Int"
  }
  
  case object TBoolean extends Type {
    override def toString = "Bool"
  }
  
  case object TString extends Type {
    override def toString = "String"
  }
  
  case object TIntArray extends Type {
    override def toString = "Int[]"
  }
  
  case class TClass(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = ??? // TODO
    override def toString = classSymbol.name
  }

  // The top of the class hierarchy. Does not correspond to anything in a Tool program,
  // we just use if for convenience during type checking.
  val TObject = TClass(new ClassSymbol("Object"))
}
