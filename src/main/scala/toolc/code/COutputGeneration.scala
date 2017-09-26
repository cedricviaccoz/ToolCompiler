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
  
  /** 
   *  take care of giving uniquely named intermediate variables
   */
  object tmpVarGen{
    private var counter = 0
    private var lastSuffix = ""
    def getFreshVar(suffix: Option[String]): String = {
      counter += 1
      val sffx = suffix getOrElse ""
      lastSuffix = sffx
      return "tmp"+sffx+counter
    }
    def getLastVar: String = "tmp"+lastSuffix+counter
  }

  
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
    val intMaxLength = "INT_MAX_LENGTH"
    macros.append("#define "+intMaxLength+" 12\n")

    //add number corresponding to each classes
    var numberClass = 0
    while(numberClass < prog.classes.size) {
      macros.append("#define n"+ prog.classes(numberClass).id.value +" "+ numberClass +"\n")
      numberClass += 1
    }    
    
    def genMethods(ct: ClassDecl): StringBuilder = 
        (for(mt <- ct.methods)yield(cGenMethod(ct, mt))).foldLeft(new StringBuilder())((a,b) => a append b)
        
    def genCMethName(className: String, methodName: Identifier ): String = className+"_"+methodName.value
    
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
                val placeToUpdate = parentMembers.indexWhere { m => (m.getName equals mt.id.value) && (m match{case _ :StructFunctionPtr => true case _ => false})}
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
     * will generate an internal representation
     * of the function ptr of the struct.
     * The "void * this" argument is not included here, it
     * is generated at the toStringRepr side.
     * 
     */
    def genStructFunctPtr(mt: MethodDecl): StructFunctionPtr = {
      val ptr = new FunctionPtr(mt.id.value, 
                                toCType(mt.retType.getType),
                                mt.args.map { arg => toCType(arg.tpe.getType)})
      
      return new StructFunctionPtr(ptr, mt)
    }

    def genMainMethod(main: MainObject): StringBuilder = {
      val mainMethod = new StringBuilder("int main(void){\n")
      main.stats.foldLeft(mainMethod)((sB, stmt) => sB append(cGenStat(stmt)(1, None))) 
      return mainMethod.append("\treturn 0;\n}")
    }
    
    //this object represents the construction of the default constructor method called "new" 
    object defaultConstructor{
      
      val funcBeginning: StringBuilder = 
        new StringBuilder("void * new(int type){\n"+
                            "\tvoid * object;\n"+
                            "\tswitch(type){\n")
      
      val funcCases: String = (programStruct map addStructConstructor).mkString("\n")
      
      val funcEnding: StringBuilder = 
        new StringBuilder("\t\tdefault:\n"+
                                "\t\t\treturn NULL;\n"+
                              "\t}\n\treturn object;"
                            +"\n}\n")
      
      /**
       * Add a new case in the default constructor to 
       * allow the default allocation and correct initialization of 
       * a struct/class
       * 
       */
      def addStructConstructor(str: StructDef): StringBuilder = {
        val caseTabLvl = genTabulation(2)
        val othrTabLvl = genTabulation(3)
        val base: StringBuilder = new StringBuilder(caseTabLvl+"case n"+ str.name +":\n"+othrTabLvl+"object = malloc(sizeof(struct "+ str.name +"));")
        
        def helperAcc(member: StructMember): Unit = member match {
          case m: StructFunctionPtr => 
            base.append("\n"+othrTabLvl+"((struct "+ str.name +"*) object)->"+ m.getName +" = "+ 
                                        genCMethName(m.mtDcl.getSymbol.classSymbol.name, m.mtDcl.id)+";") 
          case _ =>
        }
        
        val el = str.membersList map helperAcc
        return base.append("\n"+othrTabLvl+"break;\n")
      }
      
      /**
       * Should be called once every tool class has been generated,
       * return the default constructor as a C method
       * "void * new(int type)" finalized.
       */
      def complete(): StringBuilder = funcBeginning.append(funcCases).append(funcEnding)
    }
   
    
    def cGenMethod(cl: ClassDecl, mt: MethodDecl): StringBuilder = {
      val parameters: StringBuilder = new StringBuilder(if(mt.args.length != 0) {", "+ mt.args.map { x => toCType(x.tpe.getType) +" "+ x.id.value }.mkString(", ")} else "")
      
      val variables: StringBuilder = new StringBuilder(mt.vars.map{ x => ("\t"+ toCType(x.tpe.getType) +" "+ x.id.value +";") }.mkString("\n"))
      
      val meth: StringBuilder = new StringBuilder("\n"+ toCType(mt.retType.getType) +" "+ cl.id.value +"_"+ mt.id.value + " ("+
           CStruct.toString()+" this"+ parameters +") {\n") 
      
      meth.append(variables.append("\n"))
      mt.stats.foldLeft(meth)((sB, stmt) => sB append(cGenStat(stmt)(1, Some(mt))))
      val tmpRetExpr = cGenExpr(mt.retExpr)(1, Some(mt))
      meth.append(tmpRetExpr)
      val retExprVar = tmpVarGen.getLastVar
      meth.append("\treturn "+retExprVar  +";") 
      return meth.append("\n}\n")
    }
    
    // Generates code for a statement
    def cGenStat(statement: StatTree)(implicit indentLvl: Int, mt: Option[MethodDecl]): StringBuilder = {
      statement match {
        case Block(stats) =>
          val currTab = genTabulation(indentLvl)
          stats.foldLeft(new StringBuilder)((sB, stmt) => sB append cGenStat(stmt)(indentLvl + 1, mt))
          
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          val currentTab = genTabulation(indentLvl)
          val exprString = cGenExpr(expr)(indentLvl, mt)
          val exprLastVar = tmpVarGen.getLastVar
          val ifPart = new StringBuilder(currentTab+"if(" +
              exprLastVar + 
              "){\n" +
              cGenStat(thn)(indentLvl + 1, mt)+currentTab+"}\n")
              
          val elsePart = {
            els match {
              case Some(el) => new StringBuilder(currentTab+"else {\n" +
                  cGenStat(el)(indentLvl+1, mt) +currentTab+"}\n")
              case None => ""
            }
          }
          return exprString.append(ifPart).append(elsePart) 
          
        case While(expr: ExprTree, stat: StatTree) =>
          val currTab = genTabulation(indentLvl)
          val exprString = cGenExpr(expr)
          val exprLastVar = tmpVarGen.getLastVar
          val whileResultVar = new StringBuilder(currTab+"while("+
              exprLastVar +
              "){\n" +
              cGenStat(stat)(indentLvl+1, mt)+"\n")
          /*
           * Now in our model, the condition will be evaluated only one time and
           * be stocked in a variable, thus the while loop will always evaluate its condition
           * with a variable whose value is not changed, thus either loop infintely or never execute the loops code.
           * Thus, we need to reevaluate our condition at the end of each loop
           */
          val reevaluationExpr = cGenExpr(expr)(indentLvl + 1, mt)
          val reevaluationRes = tmpVarGen.getLastVar
          val reevaluationAssign = currTab+tab+exprLastVar+" = "+reevaluationRes+";\n"+currTab+"}\n"
          return exprString.append(whileResultVar).append(reevaluationExpr).append(reevaluationAssign)
          
        case Println(expr: ExprTree) =>
          val exprString = cGenExpr(expr)
          val exprLastVar = tmpVarGen.getLastVar
          val innerPrint: String = expr.getType match {
            case TInt | TBoolean => "\"%d\\n\""
            case TString => "\"%s\\n\""
            case _ => sys.error("The parameter's type of the function println() is incorrect.")
          }
          val printlnResultVar = genTabulation(indentLvl) + "printf("+ innerPrint +", "+ exprLastVar +");\n"
          return exprString.append(printlnResultVar)
          
        case Assign(id: Identifier, expr: ExprTree) =>
          //we need to determine if our variable to be assigned is a struct field or a method variable.
          val toAssign = determineVarOrField(id, mt)
          val exprString = cGenExpr(expr)
          val exprLastVar = tmpVarGen.getLastVar
          val exprResultVar = genTabulation(indentLvl) + toAssign + " = " + exprLastVar + ";\n"
          return exprString.append(exprResultVar)
          
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          val arrayId = determineVarOrField(id, mt)
          val indexString = cGenExpr(index)
          val indexLastVar = tmpVarGen.getLastVar
          val exprString = cGenExpr(expr)
          val exprLastVar = tmpVarGen.getLastVar
          val exprResultVar = genTabulation(indentLvl) + arrayId + "[" + indexLastVar + "] = " + exprLastVar +";\n"
          return indexString.append(exprString).append(exprResultVar)
          
        case DoExpr(e: ExprTree) =>
          val eString = cGenExpr(e)
          val eLastVar = tmpVarGen.getLastVar
          val exprResultVar = genTabulation(indentLvl) + eLastVar +";\n"
          return eString.append(exprResultVar)
          
        case _ => sys.error("Unknown Statement evaluation at compilation time.")
      }
    }
    
    // Generates code for an expression
    def cGenExpr(expr: ExprTree)(implicit indentLvl: Int, mt: Option[MethodDecl]): StringBuilder = {
      expr match {
        case And(lhs, rhs) =>
          operatorExprBuilder(CInt, lhs, rhs, " && ", "", Some("And"))
          
        case Or(lhs, rhs) =>
          operatorExprBuilder(CInt, lhs, rhs, " || ", "", Some("And"))
          
        case Not(expr: ExprTree) =>
          singleExprBuilder(CInt, expr, "!", "", Some("Not"))

        // Arithmetic operators (Plus works on any combination of Int/String)
        case Plus(lhs: ExprTree, rhs: ExprTree) => (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            operatorExprBuilder(CInt, lhs, rhs, " + ", "", Some("Plus"))
                      
          case (TInt, TString) =>
            val lhsString = cGenExpr(lhs)
            val lhsLastVar = tmpVarGen.getLastVar
            val rhsString = cGenExpr(rhs)
            val rhsLastVar = tmpVarGen.getLastVar
            
            val concat = "strcat(strcpy(malloc(strlen("+ lhsLastVar +") + strlen(itoa(" + rhsLastVar +")) + 1), itoa(" +
                lhsLastVar +"))," + rhsLastVar +")"
            val plusExprResultVar = genTabulation(indentLvl)+CString.toString()+" "+tmpVarGen.getFreshVar(Some("CatIS"))+" = "+concat+";\n"
            return lhsString.append(rhsString).append(plusExprResultVar)
            
          case (TString, TInt) => 
            val lhsString = cGenExpr(lhs)
            val lhsLastVar = tmpVarGen.getLastVar
            val rhsString = cGenExpr(rhs)
            val rhsLastVar = tmpVarGen.getLastVar
            
            val concat = "strcat(strcpy(malloc(strlen("+ lhsLastVar +") + strlen(itoa("+ rhsLastVar +")) + 1), "+ 
                lhsLastVar +"), itoa("+ rhsLastVar +"))"
            val plusExprResultVar = genTabulation(indentLvl)+CString.toString()+" "+tmpVarGen.getFreshVar(Some("CatSI"))+" = "+concat+";\n"
            return lhsString.append(rhsString).append(plusExprResultVar)
            
          case (TString, TString) =>
            val lhsString = cGenExpr(lhs)
            val lhsLastVar = tmpVarGen.getLastVar
            val rhsString = cGenExpr(rhs)
            val rhsLastVar = tmpVarGen.getLastVar
            
            val concat = "strcat(strcpy(malloc(strlen("+ lhsLastVar +") + strlen(" + rhsLastVar +") + 1)," +
                lhsLastVar +")," + rhsLastVar +")"
            val plusExprResultVar = genTabulation(indentLvl)+CString.toString()+" "+tmpVarGen.getFreshVar(Some("CatSS"))+" = "+concat+";\n"
            return lhsString.append(rhsString).append(plusExprResultVar)
                    
          case _                  => sys.error("addition between two incompatible types at code generation !")
        }
        
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
            operatorExprBuilder(CInt, lhs, rhs, " - ", "", Some("Min"))
          
        case Times(lhs: ExprTree, rhs: ExprTree) =>
            operatorExprBuilder(CInt, lhs, rhs, " * ", "", Some("Tim"))
          
        case Div(lhs: ExprTree, rhs: ExprTree) =>
            operatorExprBuilder(CInt, lhs, rhs, " / ", "", Some("Div"))
          
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
            operatorExprBuilder(CInt, lhs, rhs, " < ", "", Some("Lt"))

        // Equality
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
            operatorExprBuilder(CInt, lhs, rhs, " == ", "", Some("Eq"))
          
        // Array expressions
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
           operatorExprBuilder(CInt, arr, index, "[", "]", Some("ArrRd"))
          
        case ArrayLength(arr: ExprTree) =>
          singleExprBuilder(CInt, arr, "*(", "-1)", Some("ArrLng"))
          
        case NewIntArray(size: ExprTree) =>
          singleExprBuilder(CIntArray, size, "arrayAlloc(", ")", Some("NArr"))
          
        // Object-oriented expressions
        case This() =>
          return new StringBuilder(genTabulation(indentLvl)+"void *"+tmpVarGen.getFreshVar(Some("This"))+" = this;\n")
        
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          
          val objExprString = cGenExpr(obj)
          val objLastVar = tmpVarGen.getLastVar
          
          var arguments = new ListBuffer[String]
          arguments.append(objLastVar)
          
          val argsCode = (for(a <- args)yield{
            val argExpr = cGenExpr(a)
            arguments append (tmpVarGen.getLastVar)
            argExpr
          }).foldLeft(new StringBuilder)((sB, argStrB) => sB append argStrB)
          
          val retType = toCType(meth.getType).toString()
          
          val structCast = obj.getType match{
            case TClass(c) => c.name
            case _  => sys.error("Calling method on a non object field.")
          }
          
          val methodCallString = "((struct "+structCast+" *)"+objLastVar+")->"+ meth.value +"("+ arguments.mkString(", ") +")"
          return objExprString.append(argsCode).append(genTabulation(indentLvl)+retType+" "+tmpVarGen.getFreshVar(Some("MetCal"))+" = "+methodCallString+";\n")
          
        case New(tpe: Identifier) =>
          return new StringBuilder(genTabulation(indentLvl)+"void * "+tmpVarGen.getFreshVar(Some("NewCl"))+" = new(n"+ tpe.value.toString() +");\n")
          
        // Literals
        case IntLit(value: Int) => 
          return new StringBuilder(genTabulation(indentLvl)+"int "+tmpVarGen.getFreshVar(Some("Int"))+" = "+value.toString+";\n")
          
        case StringLit(value: String) =>
          return new StringBuilder(genTabulation(indentLvl)+"char * "+tmpVarGen.getFreshVar(Some("Str"))+" = \""+value+"\";\n")
          
        case True() => 
          return new StringBuilder(genTabulation(indentLvl)+"int "+tmpVarGen.getFreshVar(Some("Tru"))+" = 1;\n")
          
        case False() => 
          return new StringBuilder(genTabulation(indentLvl)+"int "+tmpVarGen.getFreshVar(Some("Fals"))+" = 0;\n")
          
        case Variable(id: Identifier) =>
          val varTrueName = determineVarOrField(id, mt)
          return new StringBuilder(genTabulation(indentLvl)+toCType(id.getType).toString+" "+tmpVarGen.getFreshVar(Some("Var"))+" = "+varTrueName+";\n")
      }
    }
    
    /** Utilitary/Helper methods **/
    
    //Helper function that construct the tmp variable satement holding an operator expression result. This serves to avoid repeating code.
    def operatorExprBuilder(tpe: CType, lhs: ExprTree, rhs: ExprTree, operator: String, endOfLine: String, varName: Option[String])(implicit indentLvl: Int, mt: Option[MethodDecl]): StringBuilder = {
      val lhsString = cGenExpr(lhs)
      val lhsLastVar = tmpVarGen.getLastVar
      val rhsString = cGenExpr(rhs)
      val rhsLastVar =  tmpVarGen.getLastVar
      return lhsString.append(rhsString).append(genTabulation(indentLvl)+tpe.toString+" "+tmpVarGen.getFreshVar(varName)+" = "+lhsLastVar+operator+rhsLastVar+endOfLine+";\n")
    }
    
    //Same as above but when only one action is required on an expression.
    def singleExprBuilder(tpe: CType, expr: ExprTree, action: String, endOfLine: String, varName: Option[String])(implicit indentLvl: Int, mt: Option[MethodDecl]): StringBuilder = {
      val exprString = cGenExpr(expr)
      val exprLastVar = tmpVarGen.getLastVar
      return exprString.append(genTabulation(indentLvl)+tpe.toString+" "+tmpVarGen.getFreshVar(varName)+" = "+action+exprLastVar+endOfLine+";\n")
    }
    
    /*
     * Helper method which return the number given as argument as a chain of char tabulation.
     */
    def genTabulation(indentLevel: Int):String = (0 until indentLevel).foldLeft(new StringBuilder)((Sb, i) => Sb append tab).toString
    
   
    /**
     * Utilitary function that according to the id given as an argument,
     * will take care or returning it as simply a variable if the id refer a method
     * variable, or the field of the corresponding struct
     * 
     */
    def determineVarOrField(id: Identifier, mt: Option[MethodDecl]): String = mt match {
      case Some(meth) => 
        if (meth.vars.map{_.id }.contains(id) || meth.args.map{_.id }.contains(id)) {
          // part of the variables or of the arguments of the method
          id.value.toString()
        } else {
          // otherwise it is part of the object itself
          val methClassName = meth.getSymbol.classSymbol.name // we need to retrieve the class from where this method stem to cast the this pointer.
          "((struct "+methClassName+"*)this)->"+ id.value.toString()
        }          
      case None => sys.error("Using variable/argument in non variable/argument context at compilation time")
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
    
    
    //we need an header to be able to call New from every method's body
    val headerFileName = outputName.dropRight(1)+"h"
    macros.append("#include \""+headerFileName+"\"\n")
    
    val headerFileCode = "#ifndef "+outputName.dropRight(2).toUpperCase()+"_H_\n#define "+outputName.dropRight(2).toUpperCase()+"_H_\n"+
                          "void * new(int type);\n\n#endif"

    // output struct corresponding to the original Tool classes
    val structs =  prog.classes.foldLeft(new StringBuilder)((sB, cl) => sB append genStructDef(cl).toStringRepr)
    
    val methods = prog.classes.foldLeft(new StringBuilder)((sB, cl) => sB append genMethods(cl))

    // output main object code
    val cMainMethod = genMainMethod(prog.main)
    
    // Two helper functions for the concatenation of a string of characters and an int
    val helperReverseFunction: StringBuilder = new StringBuilder(
        "\n\n// helper functions for the concatenation of a string of characters and an int:\n"+
        "void helper_reverse_plus(char str[], int len) {\n"+
        "\tint start;\n"+
        "\tint end;\n"+
        "\tchar temp;\n"+
        "\tfor(start = 0, end = len-1; start < end; start++, end--) {\n"+
        "\t\ttemp = *(str+start);\n"+
        "\t\t*(str+start) = *(str+end);\n"+
        "\t\t*(str+end) = temp;\n"+
        "\t}\n"+
        "}"+
        "\n"
    )
    
    val helperItoaFunction: StringBuilder = new StringBuilder(
        "char* itoa(int num) {\n"+
        "\tint i = 0;\n"+
        "\tint isNegative = 0;\n"+
        "\tchar* str = malloc("+intMaxLength+");\n"+
        "\n"+
        "\tif (num == 0) {\n"+
        "\t\tstr[i] = '0';\n"+
        "\t\tstr[i + 1] = '\\0';\n"+
        "\t\treturn str;\n"+
        "\t}\n"+
        "\n"+
        "\tif (num < 0) {\n"+
        "\t\tisNegative = 1;\n"+
        "\t\tnum = -num;\n"+
        "\t}\n"+
        "\n"+
        "\twhile (num != 0) {\n"+
        "\t\tint rem = num % 10;\n"+
        "\t\tstr[i++] = (rem > 9) ? (rem - 10) + 'A' : rem + '0';\n"+
        "\t\tnum = num/10;\n"+
        "\t}\n"+
        "\n"+
        "\tif (isNegative) {\n"+
        "\t\tstr[i++] = '-';\n"+
        "\t}\n"+
        "\n"+
        "\tstr[i] = '\\0';\n"+
        "\thelper_reverse_plus(str, i);\n"+
        "\treturn str;\n"+
        "}"+
        "\n"
     )
    
    /**
     * Smartly allocate an array of int and keeps its length in -1 place. 
     * (Since sizeof doesnt work to compute length of an array that is a pointer)
     */
    val arrayAllocFunc: StringBuilder = new StringBuilder(
      "int * arrayAlloc(int size){\n"+
      "\tint * smrtArray = calloc(size + 1, sizeof(int));\n"+
      "\tsmrtArray[0] = size;\n"+
      "\treturn (smrtArray + 1);\n"+
      "}"+
      "\n"
    )
    
    val CProgram: String = new StringBuilder(stdLibImports)
                            .append(macros+"\n")
                            .append(structs+"\n")
                            .append(helperReverseFunction)
                            .append(helperItoaFunction)
                            .append(arrayAllocFunc)
                            .append(methods+"\n")
                            .append(defaultConstructor.complete()+"\n")
                            .append(cMainMethod).toString
      
    new PrintWriter(outputName) { write(CProgram); close }
    new PrintWriter(headerFileName) { write(headerFileCode); close}
  
    /**
     * then on a terminal just execute "gcc -o toolCProg <NameOfYourFileWithoutTool>Tool.c" 
     * and then "./toolCProg" and tadam ! you have your tool program produced in C code working just as it worked on the JVM !
     */
  }
}