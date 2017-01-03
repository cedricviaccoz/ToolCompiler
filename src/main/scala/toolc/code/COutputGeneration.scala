package toolc
package code

import code.CDataType._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import utils._
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object COutputGeneration extends Pipeline[Program, Unit] {

  
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
    
    //list of all struct definitions generated by the program
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
    
    
    def genStructAndMethods(ct: ClassDecl): StringBuilder = 
      (for(mt <- ct.methods)yield(cGenMethod(ct, mt))).foldLeft(new StringBuilder(genStructDef(ct).toStringRepr))((a,b) => a append b)
    
    def genCMethName(className: Identifier, methodName: Identifier ): String = className.value+"_"+methodName.value
    
    /**
     * takes care of generating a C Struct internal definition according
     * to the tool ClassDecl given as argument.
     */
    def genStructDef(ct: ClassDecl): StructDef = {
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
              genStructDef(parentClassDecl).getlistOfMembers
            case None => new ListBuffer[StructMember]
          }
         
          //adding method as StructFunctionPtr to our struct 
          for(mt <- ct.methods){
            mt.getSymbol.overridden match {
              case Some(_) =>
                //in case of overrides, we need to update our list of members with the correct 
                //method declaration associated with its function pointer
                val toUpdate = genStructFunctPtr(mt)
                val placeToUpdate = parentMembers.indexWhere { m => m.getName equals mt.id.value }
                parentMembers.update(placeToUpdate, toUpdate)
              case None => parentMembers append genStructFunctPtr(mt)
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
    
    /**
     * Given the AST methodDecl given as argument, 
     * will generate 
     * 
     */
    def genStructFunctPtr(mt: MethodDecl): StructFunctionPtr = {
      val ptr = new FunctionPtr(mt.id.value, 
                                toCType(mt.retType.getType),
                                //Adding a CStruct type at the beginning to account for "this" calls.
                                mt.args.map { arg => toCType(arg.tpe.getType)} ++ List(CDataType.CStruct))
      
      return new StructFunctionPtr(ptr, mt)
    }

    def genMainMethod(main: MainObject): StringBuilder = {
      val mainMethod = new StringBuilder("int main(void){\n")
      main.stats.foldLeft(mainMethod)((sB, stmt) => sB append(cGenStat(stmt)+ ";")) // TODO need to discuss abou the way to handle \t, \n and ;
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
    def cGenMethod(cl: ClassDecl, mt: MethodDecl): StringBuilder = {
      val meth: StringBuilder = new StringBuilder("\t\t"+ toCType(mt.retType.getType) + mt.id.value + " ("+
          mt.args.map { x => toCType(x.tpe.getType) +" "+ x.id.value }.mkString(",") + ") {\n\t\t\t\t") 
      
      mt.stats.foldLeft(meth)((sB, stmt) => sB append(cGenStat(stmt) +";"))
      
      return meth.append("\t\t}\n")
    }
    
    
    // Generates code for a statement
    def cGenStat(statement: StatTree): StringBuilder = {
      statement match {
        case Block(stats) =>
          stats.foldLeft(new StringBuilder)((sB, stmt) => sB append cGenStat(stmt))
          
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          val a = new StringBuilder("\n\t\tif(" +
              cGenExpr(expr) + 
              ") {\n\t\t\t\t" +
              cGenStat(thn) +
              ";\n\t\t}")
          val b = {
            els match {
              case Some(el) => new StringBuilder(" else {\n" +
                  cGenStat(el) +
                  ";\n\t\t}")
              case None => new StringBuilder()
            }
          }
          return a.append(b) 
          
        case While(expr: ExprTree, stat: StatTree) =>
          return new StringBuilder("\n\t\twhile("+
              cGenExpr(expr) +
              ") {\n\t\t\t\t" +
              cGenStat(stat) +
              ";\n\t\t\t\t}")
          
        case Println(expr: ExprTree) =>
          return new StringBuilder("printf("+ cGenExpr(expr) +");\n")
          
        case Assign(id: Identifier, expr: ExprTree) =>
          return new StringBuilder(id.value +" = "+ cGenExpr(expr) +";\n")
          
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          ??? // TODO 
          
        case DoExpr(e: ExprTree) =>
          return cGenExpr(e)
          
        case _ => sys.error("Unknown Statement evaluation at compilation time.")
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)/*(implicit mt: MethodDecl)*/: StringBuilder = {
      expr match {
        case And(lhs, rhs) =>
          return new StringBuilder(cGenExpr(lhs) + " && "+ cGenExpr(rhs))
          
        case Or(lhs, rhs) =>
          return new StringBuilder(cGenExpr(lhs) + " || "+ cGenExpr(rhs))
          
        case Not(expr: ExprTree) =>
          return new StringBuilder("!"+ cGenExpr(expr))

        // Arithmetic operators (Plus works on any combination of Int/String)
        case Plus(lhs: ExprTree, rhs: ExprTree) => (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            return new StringBuilder(cGenExpr(lhs) +" + "+ cGenExpr(rhs))
          case (TInt, TString) | (TString, TInt) | (TString, TString) => 
            ??? // TODO
          case _                  => sys.error("addition between two incompatible types at code generation !")
        }
        
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          return new StringBuilder(cGenExpr(lhs) +" - "+ cGenExpr(rhs))

        case Times(lhs: ExprTree, rhs: ExprTree) =>
          return new StringBuilder(cGenExpr(lhs) +" * "+ cGenExpr(rhs)) 
          
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          return new StringBuilder(cGenExpr(lhs) +" / "+ cGenExpr(rhs))
          
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          return new StringBuilder(cGenExpr(lhs) +" < "+ cGenExpr(rhs))

        // Equality
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          ??? // TODO 
          
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
          new StringBuilder(value)
          
        case StringLit(value: String) =>
          new StringBuilder(value)
          
        case True() => 
          new StringBuilder("1")
          
        case False() => 
          new StringBuilder("0")
          
        case Variable(id: Identifier) =>
          new StringBuilder(id.value)
          
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
    val structAndMethods =  prog.classes.foldLeft(new StringBuilder)((sB, cl) => sB append genStructAndMethods(cl))

    // output main object code
    val cMainMethod = genMainMethod(prog.main)
    
    val CProgram: String = new StringBuilder(stdLibImports)
                            .append(macros+"\n")
                            .append(structAndMethods+"\n")
                            .append(defaultConstructor.complete()+"\n")
                            .append(cMainMethod).toString
      
    new PrintWriter(outputName) { write(CProgram); close }

  }

}

