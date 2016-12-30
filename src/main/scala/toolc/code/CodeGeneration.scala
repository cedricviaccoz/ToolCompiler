package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String, Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/

    def generateClassFile(ct: ClassDecl, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor
      
      for(v <- ct.vars) cf.addField(typeToDescr(v.tpe.getType), v.getSymbol.name)
      for(m <- ct.methods){
        val ch = cf.addMethod(typeToDescr(m.retType.getType), m.id.value, m.args map {a => typeToDescr(a.getSymbol.getType)}).codeHandler
        cGenMethod(ch, m)
      }
      
      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name)

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      val cs = mt.getSymbol.classSymbol.name

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map {
        case (arg, index) =>
          arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map(v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings
      
      mt.stats foreach (x => cGenStat(x)(ch,mapping, cs))
      
      ch << LineNumber(mt.retExpr.line)  
      cGenExpr(mt.retExpr)(ch, mapping, cs)
      
      ch << (typeToDescr(mt.retExpr.getType) match{
        case "I" | "Z" => IRETURN
        case _ => ARETURN 
      })
      
      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      stmts foreach {s => cGenStat(s)(ch, Map.empty, cname)}
      ch << RETURN
      ch.freeze
    }
    
    // Generates code for a statement
    def cGenStat(statement: StatTree)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) =>
          stats foreach cGenStat
          
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          ch << LineNumber(statement.line)
          cGenExpr(expr)
          val outOfIf = ch.getFreshLabel("outOfIf")
          els match{
            case Some(elseStat) =>
              val elsLabel = ch.getFreshLabel("elsLabel")
              ch << IfEq(elsLabel)
              cGenStat(thn)
              ch << Goto(outOfIf)
              ch << Label(elsLabel)
              cGenStat(elseStat)
            case None =>
              ch << IfEq(outOfIf)
              cGenStat(thn)
          }
          ch << Label(outOfIf)
          
          
        case While(expr: ExprTree, stat: StatTree) =>
          ch << LineNumber(statement.line)
          val whileLabel = ch.getFreshLabel("whileLabel")
          ch << Label(whileLabel)
          cGenExpr(expr)
          val loopLabel = ch.getFreshLabel("loopLabel")
          val outOfLoop = ch.getFreshLabel("OutOfLoop")
          ch << IfNe(loopLabel)
          ch << Goto(outOfLoop)
          ch << Label(loopLabel)
          cGenStat(stat)
          ch << Goto(whileLabel)
          ch << Label(outOfLoop)
          
        case Println(expr: ExprTree) =>
          ch << LineNumber(statement.line)
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "("+typeToDescr(expr.getType)+")V")
          
        case Assign(id: Identifier, expr: ExprTree) =>
          ch << LineNumber(statement.line)
          mapping.get(id.getSymbol.name) match {
            case Some(slot) =>
              cGenExpr(expr)
              typeToDescr(expr.getType) match {
                case "I" | "Z" => ch << IStore(slot)
                case _ => ch << AStore(slot)
              }
            case None => 
              ch << ALoad(0)
              cGenExpr(expr)
              ch << PutField(cname, id.getSymbol.name, typeToDescr(expr.getType))  
          }
          
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          ch << LineNumber(statement.line)
          val arrSymb = id.getSymbol.name
          if(mapping contains arrSymb) ch << ALoad(mapping(arrSymb))
          else{
            ch << ALoad(0)
            ch << GetField(cname, arrSymb, typeToDescr(TIntArray))
          }
          cGenExpr(index)
          cGenExpr(expr)
          ch << IASTORE
          
        case DoExpr(e: ExprTree) =>
          ch << LineNumber(statement.line)
          cGenExpr(e)
          //we discard the result given, and only keep the code for its side effect.
          ch << POP
        case _ => sys.error("Unknown Statement evaluation at compilation time.")
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs, rhs) =>
          ch << ICONST_0
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)

        case Or(lhs, rhs) =>
          ch << ICONST_1
          cGenExpr(lhs)

          val trueLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(trueLabel)

          ch << POP
          cGenExpr(rhs)

          ch << Label(trueLabel)
          
        case Not(expr: ExprTree) =>
          cGenExpr(expr)
          val invLabel = ch.getFreshLabel("invLabel")
          val nextLabel = ch.getFreshLabel("notNextLabel")
          
          ch << IfNe(invLabel)
          ch << ICONST_1
          ch << Goto(nextLabel)
          
          ch << Label(invLabel)
          ch << ICONST_0
          
          ch << Label(nextLabel)

        // Arithmetic operators (Plus works on any combination of Int/String)
        case Plus(lhs: ExprTree, rhs: ExprTree) => (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            cGenExpr(lhs)
            cGenExpr(rhs)
            ch << IADD
          case (TInt, TString) | (TString, TInt) | (TString, TString) => 
            val strBldr = "java/lang/StringBuilder"
            ch << DefaultNew(strBldr)
            cGenExpr(lhs)
            ch << InvokeVirtual(strBldr, "append", "("+typeToDescr(lhs.getType)+")L"+strBldr+";")
            cGenExpr(rhs)
            ch << InvokeVirtual(strBldr, "append", "("+typeToDescr(rhs.getType)+")L"+strBldr+";")
            ch << InvokeVirtual(strBldr, "toString", "()Ljava/lang/String;") 
          case _                  => sys.error("addition between two incompatible types at code generation !")
        }
        
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << ISUB

        case Times(lhs: ExprTree, rhs: ExprTree) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IMUL

        case Div(lhs: ExprTree, rhs: ExprTree) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IDIV

        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          cGenExpr(lhs)
          cGenExpr(rhs)
          
          val greaterEqualLabel = ch.getFreshLabel("greaterEqualLabel")
          val ltNextLabel = ch.getFreshLabel("ltNextLabel")
          ch << If_ICmpGe(greaterEqualLabel)
          
          ch << ICONST_1
          ch << Goto(ltNextLabel)
          
          ch << Label(greaterEqualLabel)
          ch << ICONST_0
          
          ch << Label(ltNextLabel)

        // Equality
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          val equalLabel = ch.getFreshLabel("equalsLabel")
          val afterEqual = ch.getFreshLabel("afterEquals")
          cGenExpr(rhs)
          cGenExpr(lhs)
          (lhs.getType, rhs.getType) match{
            case (TInt, TInt) | (TBoolean, TBoolean)  =>
              ch << If_ICmpEq(equalLabel)
            case _ =>
              ch << If_ACmpEq(equalLabel)
          }
          ch << ICONST_0
          ch << Goto(afterEqual)
          ch << Label(equalLabel)
          ch << ICONST_1
          ch << Label(afterEqual)
          
        // Array expressions
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          cGenExpr(arr)
          cGenExpr(index)
          ch << IALOAD

        case ArrayLength(arr: ExprTree) =>
          cGenExpr(arr)
          ch << ARRAYLENGTH

        case NewIntArray(size: ExprTree) =>
          //SelfNote: T_INT value is 10
          cGenExpr(size)
          ch << NewArray.primitive("T_INT")
          
        // Object-oriented expressions
        case This() => ch << ALoad(0)
        
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          cGenExpr(obj)
          for(arg <- args) cGenExpr(arg)
          val objName = obj.getType toString
          val methSign = "("+args.foldLeft(new StringBuilder)((sB, a) => sB.append(typeToDescr(a.getType))).toString + ")"+typeToDescr(meth.getType)
          val functName = meth.value
          ch << InvokeVirtual(objName, functName, methSign)
          
        case New(tpe: Identifier) =>
          ch << DefaultNew(tpe.value)
          
          
        // Literals
        case IntLit(value: Int) => 
          ch << Ldc(value)
          
        case StringLit(value: String) =>
          ch << Ldc(value)
          
        case True() => 
          ch << ICONST_1
          
        case False() => 
          ch << ICONST_0
          
        case Variable(id: Identifier) =>
          mapping.get(id.getSymbol.name) match {
          //in the case of either a method argument or a intermediate variable is sought.
           case Some(slot) =>  
             typeToDescr(expr.getType) match {
              case "I" | "Z" => ch << ILoad(slot)
              case _ => ch << ALoad(slot)
             }
           case None =>
            //in the case we seek a class field
            ch << ALoad(0)
            ch << GetField(cname, id.getSymbol.name, typeToDescr(expr.getType))
          }
      }

    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
      case TInt      => "I"
      case TBoolean  => "Z"
      case TClass(symb) => "L"+symb.name + ";"
      case TIntArray => "[I"
      case TString   => "Ljava/lang/String;"
      case _         => sys.error("Unknown type at compilation time")
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)

  }

}

