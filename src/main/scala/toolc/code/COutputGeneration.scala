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
    //[BEGIN]
    //add number corresponding to each classes
    var numberClass = 0
    while(numberClass < prog.classes.size) {
      macros.append("#define n"+ prog.classes(numberClass).id.value +" "+ numberClass +"\n")
      numberClass += 1
    }
    //[END]
    
    def genStructAndMethods(ct: ClassDecl): StringBuilder = 
      (for(mt <- ct.methods)yield(cGenMethod(ct, mt))).foldLeft(new StringBuilder(genStructDef(ct).toStringRepr))((a,b) => a append b)
    
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
     * will generate an internal representation
     * of the function ptr of the struct.
     * The "this *" argument is not included here, it
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
      main.stats.foldLeft(mainMethod)((sB, stmt) => sB append(cGenStat(stmt)(0, null))) 
      return mainMethod.append("\treturn 0;\n}")
    }
    
    //this object represent 
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
          case m: StructVar =>  
            //J'ai pas vraiment compris pourquoi tu veux initialiser des variables ici jeune ami nanchen. 
            //base.append("\n\t\t((struct "+ str.name +"*) object)->"+ m.getName +" = "+ m.getName +";") 
          case m: StructFunctionPtr => 
            base.append("\n"+othrTabLvl+"((struct "+ str.name +"*) object)->"+ 
                                        genCMethName(m.mtDcl.getSymbol.classSymbol.name, m.mtDcl.id)+";") 
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

      val meth: StringBuilder = new StringBuilder("\n"+ toCType(mt.retType.getType) +" "+ cl.id.value +"_"+ mt.id.value + " ("+
          "struct "+ cl.id.value +"* this"+ parameters +") {\n") 
      
      mt.stats.foldLeft(meth)((sB, stmt) => sB append(cGenStat(stmt)(1, mt)))
      meth.append("\treturn "+ cGenExpr(mt.retExpr)(1, mt) +";") 
      return meth.append("\n}\n")
    }
    
    /*
     * Helper method which return the the number given as argument as number of tabulation.
     */
    def genTabulation(indentLevel: Int):String = (0 until indentLevel).foldLeft(new StringBuilder)((Sb, i) => Sb append tab).toString
    
    // Generates code for a statement
    def cGenStat(statement: StatTree)(implicit indentLvl: Int, mt: MethodDecl): StringBuilder = {
      statement match {
        case Block(stats) =>
          val currTab = genTabulation(indentLvl)
          stats.foldLeft(new StringBuilder(currTab+"{\n"))((sB, stmt) => sB append cGenStat(stmt)(indentLvl + 1, mt)).append(currTab+"}")
          
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          val currentTab = genTabulation(indentLvl)
          val ifPart = new StringBuilder(currentTab+"if(" +
              cGenExpr(expr)(indentLvl + 1, mt) + 
              ")\n" +
              cGenStat(thn)(indentLvl + 1, mt))
              
          val elsePart = {
            els match {
              case Some(el) => new StringBuilder(currentTab+"else\n" +
                  cGenStat(el)(indentLvl+1, mt) +"\n")
              case None => new StringBuilder("\n")
            }
          }
          return ifPart.append(elsePart) 
          
        case While(expr: ExprTree, stat: StatTree) =>
          val currTab = genTabulation(indentLvl)
           return new StringBuilder(currTab+"while("+
              cGenExpr(expr) +
              ")\n" +
              cGenStat(stat)(indentLvl+1, mt)+"\n")
          
        case Println(expr: ExprTree) =>
          println(expr.getType)
          return new StringBuilder(genTabulation(indentLvl)+"printf("+ cGenExpr(expr) +");\n") // TODO revenir!!!
          
        case Assign(id: Identifier, expr: ExprTree) =>
          return new StringBuilder(genTabulation(indentLvl)+id.value +" = "+ cGenExpr(expr) +";\n")
          
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          return new StringBuilder(genTabulation(indentLvl)+ id.value +"["+ cGenExpr(index) +"] = "+ cGenExpr(expr) +";\n")
          
        case DoExpr(e: ExprTree) =>
          return new StringBuilder(genTabulation(indentLvl)+cGenExpr(e) +";\n")
          
        case _ => sys.error("Unknown Statement evaluation at compilation time.")
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)(implicit indentLvl: Int, mt: MethodDecl): StringBuilder = {
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
          case (TInt, TString) =>
            return new StringBuilder("strcat(strcpy(malloc(strlen("+ cGenExpr(lhs) +") + strlen(" + cGenExpr(rhs) +") + 1), itoa(" +
                cGenExpr(lhs) +"))," + cGenExpr(rhs) +")") 
          case (TString, TInt) => 
            return new StringBuilder("strcat(strcpy(malloc(strlen("+ cGenExpr(lhs) +") + sizeof("+ cGenExpr(rhs) +") + 1), "+ 
                cGenExpr(lhs) +"), itoa("+ cGenExpr(rhs) +"))")
          case (TString, TString) =>
            return new StringBuilder("strcat(strcpy(malloc(strlen("+ cGenExpr(lhs) +") + strlen(" + cGenExpr(rhs) +") + 1)," +
                cGenExpr(lhs) +")," + cGenExpr(rhs) +")") 
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
          return new StringBuilder(cGenExpr(lhs) + " == "+ cGenExpr(rhs)) 
          
        // Array expressions
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          return new StringBuilder(cGenExpr(arr) +"["+ cGenExpr(index) +"]")
          
        case ArrayLength(arr: ExprTree) =>
          return new StringBuilder("sizeof("+ cGenExpr(arr) +") / sizeof(int)")
          
        case NewIntArray(size: ExprTree) =>
          return new StringBuilder("calloc("+ cGenExpr(size) +", sizeof(int))")
          
        // Object-oriented expressions
        case This() =>
          return new StringBuilder("this")
        
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          val arguments = {
            for{
              a <- args
            } yield(cGenExpr(a))
          }.mkString(", ")
          return new StringBuilder(cGenExpr(obj) +"->"+ meth.value +"("+ arguments +")")
          
        case New(tpe: Identifier) =>
          return new StringBuilder("new("+ tpe.value.toString() +")")
          
        // Literals
        case IntLit(value: Int) => 
          return new StringBuilder(value.toString())
          
        case StringLit(value: String) =>
          return new StringBuilder("\"value\"")
          
        case True() => 
          return new StringBuilder("1")
          
        case False() => 
          return new StringBuilder("0")
          
        case Variable(id: Identifier) =>
          if (mt.vars.map{ x => x.id }.contains(id) || mt.args.map{ x => x.id }.contains(id)) {
            // part of the variables or of the arguments of the method
            return new StringBuilder(id.value.toString())
          } else {
            // otherwise it is part of the object itself
            return new StringBuilder("this->"+ id.value.toString())
          }          
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
    
    // Two helper functions for the concatenation of a string of characters and an int
    val helperReverseFunction: StringBuilder = new StringBuilder(
        "\n\n\n// helper functions for the concatenation of a string of characters and an int:\n\n"+
        "void helper_reverse_plus(char str[], int len) {\n\t"+
        "int start;\n\tint end;\n\tchar temp;\n\t"+
        "for(start = 0, end = len-1; start < end; start++, end--) {\n\t\t"+
        "temp = *(str+start);\n\t\t*(str+start) = *(str+end);\n\t\t*(str+end) = temp;\n\t}\n}\n\n")
    
    val helperItoaFunction: StringBuilder = new StringBuilder(
        "char* itoa(int num) {\n\t"+
        "int i = 0;\n\tint isNegative = 0;\n\tchar* str = malloc(sizeof(num));\n\n\t"+
        "if (num == 0) {\n\t\t"+
        "str[i] = '0';\n\t\tstr[i + 1] = '\\0';\n\t\treturn str;\n\t}\n\n\t"+
        "if (num < 0) {\n\t\tisNegative = 1;\n\t\tnum = -num;\n\t}\n\n\t"+
        "while (num != 0) {\n\t\tint rem = num % 10;\n\t\tstr[i++] = (rem > 9) ? (rem - 10) + 'A' : rem + '0';\n\t\tnum = num/10;\n\t}\n\n\t"+
        "if (isNegative) {\n\t\tstr[i++] = '-';\n\t}\n\n\t"+
        "str[i] = '\\0';\n\thelper_reverse_plus(str, i);\n\treturn str;\n}\n")
    
    val CProgram: String = new StringBuilder(stdLibImports)
                            .append(macros+"\n")
                            .append(structAndMethods+"\n")
                            .append(defaultConstructor.complete()+"\n")
                            .append(cMainMethod)
                            .append(helperReverseFunction)
                            .append(helperItoaFunction).toString
      
    new PrintWriter(outputName) { write(CProgram); close }

  }

}

