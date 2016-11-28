package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      if (prog.main.id.value equals "Object") error(s"Main Object cannot bear the name 'Object' !")
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      for (c <- prog.classes) {
        val className = c.id.value
        if (global.classes.contains(className)) {
          error(s"Class ${c.id.value} is already defined at position ${global.classes.get(className).get.position}")
        } else if (c.id.value == "Object") {
          error(s"the class at position ${c.position} cannot bear the name 'Object' .")
        } else if (c.id.value == prog.main.id.value) {
          error(s"the class at position ${c.position} cannot bear the same name as the main object.")
        }
        val clSymb = new ClassSymbol(className).setPos(c)
        global.classes += ((className, clSymb))
        c.setSymbol(clSymb)
        c.id.setSymbol(clSymb)
      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None           => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p)        => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }

      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      prog.classes.foreach(collectInClass)

      def collectInClass(c: ClassDecl): Unit = {
        // TODO: Traverse a class to collect symbols and emit errors
        //       in case a correctness rule of Tool is violated
        // Note: It is important that you analyze parent classes first (Why?)

        val correspClassSym = global.lookupClass(c.id.value) getOrElse sys.error("No ClassSymbol was created for this class declaration")
        if (!correspClassSym.hasBeenVisited) {

          correspClassSym.parent match {
            //case of an inherited class
            case Some(_) =>
              val parentClDecl: ClassDecl =
                prog.classes.find { p => p.id == c.parent.get } getOrElse sys.error("No class declaration is being inherited from current class declaration")
              collectInClass(parentClDecl)

              //adding the parent's inherited fields and methods
              correspClassSym.members ++= correspClassSym.parent.get.members
              correspClassSym.methods ++= correspClassSym.parent.get.methods

              //and analyzing/adding this class own's members and methods
              analyseMembers(correspClassSym, c.vars)
              for (method <- c.methods) {
               correspClassSym.lookupMethod(method.id.value) match {
                 case Some(lookp) =>
                    //duplicate method case
                    if (lookp.classSymbol == correspClassSym) {
                      error(s"Method at position ${method.position} is defined twice, first time at ${lookp.position}")
                    } else {
  
                      //we are in the overriden method state here.
                      val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                      setTypeSymbol(method.retType, global)
                      methSym.setType(method.retType.getType)
                      methSym.overridden = Some(lookp)
  
                      //testing return types are corresponding
                      if (lookp.getType != methSym.getType) {
                        error("Overriding method doesn't overrides the good return type,"
                          + s" found ${methSym.getType.toString}, expected ${lookp.getType.toString}", method)
                      }
  
                      if (method.args.size == lookp.params.size) {
                        initMethSymbol(methSym, method)
  
                        //now typechecking in the case of overriding
                        val twoMethArgs: List[(VariableSymbol, VariableSymbol)] = methSym.argList.zip(lookp.argList)
                        for (e <- twoMethArgs) {
                          if (e._1.getType != e._2.getType) {
                            error(s"Type error : argument of type ${e._1.getType.toString} of the method at position ${e._1.position}" +
                              s"is not of the same type (${e._2.getType.toString}) as argument of the overriden method at positin ${e._2.position}")
                          }
                        }
  
                        correspClassSym.methods += ((method.id.value, methSym))
                      } else {
                        //the overriden method doesn't have the same number of parameters as its parent class, need to report the error 
                        error(s"Overriden method at ${methSym.position} don't have the same number of parameter as the method it overrides at ${lookp.position}")
                      }
  
                    }
                  case None =>
                    //method is not overriden, treat it normally.
                    val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                    setTypeSymbol(method.retType, global)
                    methSym.setType(method.retType.getType)
  
                    initMethSymbol(methSym, method)
                    correspClassSym.methods += ((method.id.value, methSym))
                }
              }

            case None =>
              //case of no inherited class
              analyseMembers(correspClassSym, c.vars)

              for (method <- c.methods) {
                correspClassSym.lookupMethod(method.id.value) match {
                  case Some(a) =>
                    error(s"Method at position ${method.position} is defined twice, first time at ${a.position}")
                  case None =>
                    val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                    setTypeSymbol(method.retType, global)
                    methSym.setType(method.retType.getType)
                    initMethSymbol(methSym, method)
                    correspClassSym.methods += ((method.id.value, methSym))
                }
              }
          }
          correspClassSym.setVisited
        }
      }

      //helper method which takes care of "symbolise" the params and members of a method.
      def initMethSymbol(methSym: MethodSymbol, methodDecl: MethodDecl) = {
        for (args <- methodDecl.args) {
          if (methSym.params.contains(args.id.value)) {
            error(s"method's arguments are duplicated, first occurence found at ${methSym.params(args.id.value).position}")
          } else {
            val argSymbol = new VariableSymbol(args.id.value).setPos(args)
            setTypeSymbol(args.tpe, global)
            argSymbol.setType(args.tpe.getType)
            args.setSymbol(argSymbol)
            args.id.setSymbol(argSymbol)
            methSym.params += ((args.id.value, argSymbol))
            methSym.argList :+= argSymbol
          }
        }
        analysMethodVars(methSym, methodDecl.vars)
      }
      //makes symbol of class members and add them to the corresponding classSymbol
      def analyseMembers(currClass: ClassSymbol, vars: List[VarDecl]): Unit = for (variable <- vars) {
        val varName = variable.id.value
        if (currClass.members.contains(varName)) {
          error(s"variable at ${variable.position} is already defined at ${currClass.members.get(varName).get.position}")
        } else {
          val membSymb = new VariableSymbol(varName).setPos(variable)
          setTypeSymbol(variable.tpe, global)
          membSymb.setType(variable.tpe.getType)
          variable.setSymbol(membSymb)
          variable.id.setSymbol(membSymb)
          currClass.members += ((varName, membSymb))
        }
      }

      //makes symbol of method variables and add them to the corresponding MethodSymbol
      def analysMethodVars(currMethod: MethodSymbol, vars: List[VarDecl]): Unit = {
        for (member <- vars) {
          if (currMethod.members.contains(member.id.value)) {
            error(s"method's variable at ${member.position} is already declared at ${currMethod.members(member.id.value).position}")
          } else if (currMethod.params.contains(member.id.value)) {
            error(s"method variable at ${member.position} try to override method parameter at ${currMethod.params(member.id.value).position}")
          } else {
            val memSymb = new VariableSymbol(member.id.value).setPos(member)
            setTypeSymbol(member.tpe, global)
            memSymb.setType(member.tpe.getType)
            member.setSymbol(memSymb)
            member.id.setSymbol(memSymb)

            currMethod.members += ((member.id.value, memSymb))
          }
        }
      }

      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      // TODO: Traverse within each definition of the program
      //       and attach symbols to Identifiers and "this"
      prog.classes foreach (setCSymbols(_, gs))
      prog.main.stats foreach (setSSymbols(_)(gs, None))
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      klass.setSymbol(classSym)
      klass.id.setSymbol(classSym)
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)
        setClassVarSymbol(varDecl, classSym, gs)
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      val MethodSym = cs.lookupMethod(meth.id.value).get
      meth.setSymbol(MethodSym)
      meth.id.setSymbol(MethodSym)
      meth.args foreach (p => {
        setISymbol(p.id)(Some(MethodSym))
        setTypeSymbol(p.tpe, gs)
      })
      meth.vars foreach (p => {
        setISymbol(p.id)(Some(MethodSym))
        setTypeSymbol(p.tpe, gs)
      })
      meth.stats foreach (s => setSSymbols(s)(gs, Some(MethodSym)))
      setTypeSymbol(meth.retType, gs)
      setESymbols(meth.retExpr)(gs, Some(MethodSym))
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = stat match {
      case Block(stats: List[StatTree]) => stats foreach (setSSymbols(_))
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        setESymbols(expr)
        setSSymbols(thn)
        els match {
          case Some(e) => setSSymbols(e)
          case None    =>
        }
      case While(expr: ExprTree, stat: StatTree) =>
        setESymbols(expr)
        setSSymbols(stat)
      case Println(expr: ExprTree) => setESymbols(expr)
      case Assign(id: Identifier, expr: ExprTree) =>
        ms match {
          case Some(_) => setISymbol(id)
          case None    => error("Assignment made on the main object at " + id.position)
        }
        setESymbols(expr)
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        ms match {
          case Some(_) => setISymbol(id)
          case None    => error("Assignment made on the main object at " + id.position)
        }
        setESymbols(index)
        setESymbols(expr)
      case DoExpr(e: ExprTree) => setESymbols(e)
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = expr match {
      case And(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Or(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Plus(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Minus(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Times(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Div(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case LessThan(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case Equals(lhs: ExprTree, rhs: ExprTree) =>
        setESymbols(lhs); setESymbols(rhs)
      case ArrayRead(arr: ExprTree, index: ExprTree) =>
        setESymbols(arr); setESymbols(index)
      case ArrayLength(arr: ExprTree) => setESymbols(arr)
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        setESymbols(obj)

        //need to set Identifier for the method now that we have type checking
        obj.getType match {
          case TClass(id) =>
            gs.lookupClass(id.name) match {
              case Some(obtCl) => meth.setSymbol(obtCl.methods(meth.value))
              case None        => error("Type error: method call on a object which doesn't define this method", meth)
            }
          case _ => error("Trying to call a method on a expression which doesn't evaluate as an object type", meth)
        }
        args foreach setESymbols
      case Variable(id: Identifier) => setISymbol(id)
      case th: This =>
        ms match{
          case Some(meth) => th.setSymbol(meth.classSymbol)
          case None => error(s"Cannot call reference to 'this' at ${expr.position} on the main object")
        }
      case NewIntArray(size: ExprTree) => setESymbols(size)
      case New(tpe: Identifier) =>
        gs.lookupClass(tpe.value) match {
          case Some(cl) => tpe.setSymbol(cl)
          case None     => error(s"The class which is constructed at ${tpe.position} has not been declared.")
        }
      case Not(expr: ExprTree) => setESymbols(expr)
      case _                   =>
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = tpe match {
      case ClassType(id) =>
        gs.lookupClass(id.value) match {
          case Some(clTpe) => id.setSymbol(clTpe)
          case None        => error("Undeclared class type at " + id.position)
        }
      case _ =>

    }

    def setClassVarSymbol(tpe: VarDecl, cs: ClassSymbol, gs: GlobalScope): Unit = {
      val varSym = cs.lookupVar(tpe.id.value)
      tpe.id.setSymbol(varSym get)
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
