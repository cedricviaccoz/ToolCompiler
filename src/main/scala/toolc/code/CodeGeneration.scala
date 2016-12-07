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

      // TODO: Add class fields
      // TODO: Add class methods and generate code for them

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

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map {
        case (arg, index) =>
          arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map(v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      // TODO: generate code for statements
      // TODO: Generate code for the return expression
      // TODO: Return with the correct opcode, based on the type of the return expression

      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      // TODO: generate code for main method

      ch.freeze
    }

    // Generates code for a statement
    def cGenStat(statement: StatTree)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) =>
          stats foreach cGenStat
        
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          cGenExpr(expr)
          val outOfIf = ch.getFreshLabel("outOfIf")
          els match{
            case Some(elseStat) =>
              val elsLabel = ch.getFreshLabel("elsLabel")
              ch << IfNe(elsLabel)
              cGenStat(thn)
              ch << Goto(outOfIf)
              ch << Label(elsLabel)
              cGenStat(elseStat)
            case None =>
              ch << IfNe(outOfIf)
              cGenStat(thn)
          }
          ch << Label(outOfIf)
          
        case While(expr: ExprTree, stat: StatTree) =>
          val whileLabel = ch.getFreshLabel("whileLabel")
          ch << Label(whileLabel)
          cGenExpr(expr)
          val loopLabel = ch.getFreshLabel("loopLabel")
          val outOfLoop = ch.getFreshLabel("OutOfLoop")
          ch << IfEq(loopLabel)
          ch << Goto(outOfLoop)
          ch << Label(loopLabel)
          cGenStat(stat)
          ch << Goto(whileLabel)
          ch << Label(outOfLoop)
          
        case Println(expr: ExprTree) =>
        case Assign(id: Identifier, expr: ExprTree) => 
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        case DoExpr(e: ExprTree) =>
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
          ch << IfEq(trueLabel)

          ch << POP
          cGenExpr(rhs)

          ch << Label(trueLabel)

        case Not(expr: ExprTree) =>
          ch << ICONST_0
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
          case (TInt, TString)    => ???
          case (TString, TInt)    => ???
          case (TString, TString) => ???
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

        // Array expressions
        case ArrayRead(arr: ExprTree, index: ExprTree) =>

        case ArrayLength(arr: ExprTree) =>

        case NewIntArray(size: ExprTree) =>
        // Object-oriented expressions
        case This() =>
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        case New(tpe: Identifier) =>
        // Literals
        case IntLit(value: Int) => 
          Ldc(value)
        case StringLit(value: String) =>
        case True() => 
          ch << ICONST_1
        case False() => 
          ch << ICONST_0
        case Variable(id: Identifier) =>

      }

    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
      case TInt      => "I"
      case TBoolean  => "I"
      case TIntArray => "A"
      case TClass(_) => "A"
      case TString   => "A"
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

