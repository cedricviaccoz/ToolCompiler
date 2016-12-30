package toolc.code

import toolc.analyzer.Types
import scala.collection.mutable.ListBuffer
import toolc.ast.Trees.MethodDecl

object CDataType {
  
  //for formatting issues
  val tab = "\t"
  val bckSpace = "\n"
  
  class StructDef(val name: String, val membersList: ListBuffer[StructMember]){
      
    def addMember(mbr: StructMember) = membersList append mbr
    
    //Warning ! should be sure that a total deep copy of all elements of the membersList is being made !
    def getlistOfMembers = new ListBuffer ++ membersList
    
    def toStringRepr: String = {
      val str = new StringBuilder
      str append ("struct "+name+"{"+bckSpace)
      membersList.foldLeft(str)((sB, mbr) => sB append (tab+mbr.toStringRepr+bckSpace))
      str append ("};" + bckSpace)
      str.toString()
    }
  }
  
  trait StructMember{
    def getName: String
    def toStringRepr: String
  }
  
  class StructVar(val name: String, val tpe: CType) extends StructMember{
    override def toStringRepr = tpe.toString + " " + name + ";"
    override def getName = name
    
  }
  
  class StructFunctionPtr(val ptr: FunctionPtr, var mtDcl: MethodDecl) extends StructMember{
    override def toStringRepr = ptr.toStringRepr
    override def getName = ptr.name
  }
  
  // don't forget each function need at least as an argument a generic pointer to the struct itself. 
  class FunctionPtr(val name: String, val retType: CType, val args: List[CType]){
    def toStringRepr = 
      "("+retType.toString+")(*"+name+")("+
       args.foldRight(new StringBuilder)((arg, sB) => sB append (arg.toString+", ")).dropRight(2)+
      ");"
                      
  }
      
  /**
   * Type referring to C code 
   */
    
  sealed abstract class CType {
    def toString: String
  }

  case object CInt extends CType {
    override def toString = "int"
  }

  case object CString extends CType {
    override def toString = "char *"
  }

  case object CIntArray extends CType {
    override def toString = "int *"
  }
  
  //we don't need to name struct when using them as 
  //var, everything will be done by casting
  case object CStruct extends CType {
    override def toString = "void *"
  }
}