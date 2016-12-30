package toolc
package code

import code.CDataType._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import utils._
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object COuputGeneration extends Pipeline[Program, Unit] {

  
  //Note : it may be more clever to use streams (à la cafebabe) instead of StringBuilder to generate
  //       the source File, need investigation and discussion.
  //Note : it may be also essential to design a system to control tabulation to have a nice c program to read.
  //Note : also maybe it would be good to have a corresponding ".h" file, depending how the ast is ordered,
  //       some structs and method are not visible to the other one if we print it directly in order.
  //Note : in the context above, maybe typedef the struct ? 
  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._
    
    val tab = "\t"
    val bckSpace = "\n" 
    
    val programStruct: ListBuffer[StructDef] = new ListBuffer[StructDef]
   
    //minimum C libraries any tool program would at most need.
    val stdLibImports: String = {
      val strBldr = new StringBuilder()
      strBldr.append("#include <stdio.h>\n")
      strBldr.append("#include <string.h>\n")
      strBldr.append("#include <stdlib.h>\n")
      strBldr.toString
    } 
    
    val macros = new StringBuilder()
    //this macro needs to be defined by default in order to have correct Int to string concatenation.
    macros.append("#define INT_MAX_LENGTH 12\n")
    //must not forget to add a corresponding macro "#define n<className>" for every new class generated
    val intMaxLength = "INT_MAX_LENGTH"
    
    def generateStructAndMethods(ct: ClassDecl): StringBuilder = {
      return new StringBuilder() 
      
    }
    
    def generateStructDef(ct: ClassDecl): StructDef = {
      val currStructName = ct.id.value
      
      //first checking if the structDef has not been already generated
      programStruct.find { sD => sD.name equals currStructName } match {
        case Some(sD) => sD
        case None => {
         //then checking if the given classDeclaration inherits somebody
         val parentMembers = ct.parent match {
            case Some(c) => 
              val parentClassDecl = prog.classes.find(_.id.value equals c.value) match{
                case Some(cD) => cD
                case None => sys.error("Could not find parent class among the program class.")
              }
              generateStructDef(parentClassDecl).getlistOfMembers
            case None => new ListBuffer[StructMember]
          }
         
          //adding method as StructFunctionPtr to our Struct 
          for(mt <- ct.methods){
            mt.getSymbol.overridden match {
              case Some(_) =>
                //in case of overrides, we need to update our list of members with the correct 
                //method declaration associated with its function pointer
                val toUpdate = generateStrucFunctPtr(mt)
                val placeToUpdate = parentMembers.indexWhere { m => m.getName equals mt.id.value }
                parentMembers.update(placeToUpdate, toUpdate)
              case None => parentMembers append generateStrucFunctPtr(mt)
            }
          }
          
          //adding the vars transformed to our struct.
          for(vr <- ct.vars){
            parentMembers append new StructVar(vr.id.value, toCType(vr.tpe.getType))
          }
          
          val genStruct = new StructDef(currStructName, parentMembers)
          
          //adding this struct to a list to avoid generating two time the same one in the future.
          programStruct += genStruct
          return genStruct
        }
      }
    }
    
    def generateStrucFunctPtr(mt: MethodDecl): StructFunctionPtr = {
      val ptr = new FunctionPtr(mt.id.value, toCType(mt.retType.getType), mt.args.map { arg => toCType(arg.tpe.getType)})
      return new StructFunctionPtr(ptr, mt)
    }

    def generateMainMethod(main: MainObject): StringBuilder = {
      val mainMethod = new StringBuilder("int main(void){\n")
      main.stats.foldRight(mainMethod)((stmt, sB) => sB append(cGenStat(stmt)))
      return mainMethod.append("\treturn 0;\n}")
    }
    
    //this object represent 
    object defaultConstructor{
      
      val funcBeginning: StringBuilder = 
        new StringBuilder("void * new(int type){\n"+
                            "\tvoid * object;\n"+
                            "\tswitch(type){\n")
      
      val funcCases: StringBuilder = new StringBuilder()
      
      val funcEnding: StringBuilder = 
        new StringBuilder("\t\tdefault:\n"+
                                "\t\t\treturn NULL;\n"+
                              "\t}\n"
                            +"}\n")
      
      /**
       * Add a new case in the default constructor to 
       * allow the default allocation and correct initialization of 
       * a struct/class
       * 
       */
      def addStructConstructor(str: StructDef): Unit = {
        //TODO
      }
      
      /**
       * Should be called once every tool class has been generated,
       * return the default constructor as a C method
       * "void * new(int type)" finalized.
       */
      def complete(): StringBuilder = funcBeginning.append(funcCases).append(funcEnding)
    }
   
    
    //maybe put a classDeclaration instead of ClassSymbol here.
    def cGenMethod(cl: ClassSymbol, mt: MethodDecl): StringBuilder = {
      return new StringBuilder()
    }
    
    
    // Generates code for a statement
    def cGenStat(statement: StatTree): StringBuilder = {
      statement match {
        case Block(stats) =>
          stats.foldRight(new StringBuilder)((stmt, sB) => sB append cGenStat(stmt))
          
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          ???
          
        case While(expr: ExprTree, stat: StatTree) =>
          ???
          
        case Println(expr: ExprTree) =>
          ???
          
        case Assign(id: Identifier, expr: ExprTree) =>
          ???
          
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          ???
          
        case DoExpr(e: ExprTree) =>
          ???
          
        case _ => sys.error("Unknown Statement evaluation at compilation time.")
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)(implicit mt: MethodDecl): StringBuilder = {
      expr match {
        case And(lhs, rhs) =>
          ???
          
        case Or(lhs, rhs) =>
          ???
          
        case Not(expr: ExprTree) =>
          ???

        // Arithmetic operators (Plus works on any combination of Int/String)
        case Plus(lhs: ExprTree, rhs: ExprTree) => (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            ???
          case (TInt, TString) | (TString, TInt) | (TString, TString) => 
            ???
          case _                  => sys.error("addition between two incompatible types at code generation !")
        }
        
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          ???

        case Times(lhs: ExprTree, rhs: ExprTree) =>
          ???
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          ???
          
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          ???

        // Equality
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          ???
          
        // Array expressions
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          ???
          
        case ArrayLength(arr: ExprTree) =>
          ???
          
        case NewIntArray(size: ExprTree) =>
          ???
          
        // Object-oriented expressions
        case This() =>
          ???
        
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          ???
          
        case New(tpe: Identifier) =>
          ???
          
        // Literals
        case IntLit(value: Int) => 
          ???
          
        case StringLit(value: String) =>
          ???
          
        case True() => 
          ???
          
        case False() => 
          ???
          
        case Variable(id: Identifier) =>
          ???
          
      }

    }

    // Transforms a Tool type to the corresponding C type
    def toCType(t: Type): CType = t match {
      case TInt | TBoolean     => CInt
      case TClass(symb) => CStruct
      case TIntArray => CIntArray
      case TString   => CString
      case _         => sys.error("Unknown type at compilation time")
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file 
    val sourceName = ctx.files.head.getName
    val outputName = if(sourceName.contains(".tool")){
      sourceName.replaceAll(".tool", "Tool.c")
    }else sourceName + "Tool.c"

    // output class code in C version 
    val structAndMethods =  prog.classes.foldRight(new StringBuilder)((cl, sB) => sB append generateStructAndMethods(cl))

    // output main object code
    val cMainMethod = generateMainMethod(prog.main)
    
    val CProgram: String = new StringBuilder(stdLibImports)
                            .append(macros+"\n")
                            .append(structAndMethods+"\n")
                            .append(defaultConstructor.complete()+"\n")
                            .append(cMainMethod).toString
      
    new PrintWriter(outputName) { write(CProgram); close }

  }

}

