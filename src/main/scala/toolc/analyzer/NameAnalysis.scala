package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      for (c <- prog.classes) {
        val className = c.id.value
        if (global.classes.contains(className)) {
          error(s"Class ${c.id.value} is already defined at position ${global.classes.get(className).get.position}")
        } else if (c.id == "Object") {
          error(s"the class at position ${c.position} cannot bear the name 'Object' !")
        } else {
          val clSymb = new ClassSymbol(className).setPos(c)
          global.classes += ((className, clSymb))
          c.setSymbol(clSymb)
          
        }
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

          if (correspClassSym.parent isDefined) {
            //case of an inherited class

            val parentClDecl: ClassDecl =
              prog.classes.find { p => p.id == c.parent.get } getOrElse sys.error("No class declaration is being inherited from current class declaration")
            collectInClass(parentClDecl)

            //adding the parent's inherited fields and methods
            correspClassSym.members ++= correspClassSym.parent.get.members
            correspClassSym.methods ++= correspClassSym.parent.get.methods

            //and analyzing/adding this class own's members and methods
            analyseMembers(correspClassSym, c.vars)
            for (method <- c.methods) {
              val lookupMeth = correspClassSym.lookupMethod(method.id.value)
              if (lookupMeth isDefined) {
                //duplicate method case
                if (lookupMeth.get.classSymbol == correspClassSym) {
                  error(s"Method at position ${method.position} is defined twice, first time at ${lookupMeth.get.position}")
                } else {

                  //we are in the overriden method state here.
                  val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                  methSym.overridden = lookupMeth

                  initMethSymbol(methSym, method)
                  if (methSym.argList.size == lookupMeth.get.argList.size) {
                    correspClassSym.methods += ((method.id.value, methSym))
                  } else {
                    //the overriden method doesn't have the same number of parameters as its parent class, need to report the error 
                    error(s"Overrident method at ${methSym.position} don't have the same number of parameter as the method it overrides at ${lookupMeth.get.position}")
                  }

                }
              } else {

                //method is not overriden, treat it normally.
                val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                initMethSymbol(methSym, method)
                correspClassSym.methods += ((method.id.value, methSym))

              }
            }

          } else {
            //case of no inherited class
            analyseMembers(correspClassSym, c.vars)

            for (method <- c.methods) {
              val lookupMeth = correspClassSym.lookupMethod(method.id.value)
              if (lookupMeth isDefined) {
                error(s"Method at position ${method.position} is defined twice, first time at ${lookupMeth.get.position}")
              } else {

                val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
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
            args.setSymbol(argSymbol)
            methSym.params += ((args.id.value, argSymbol))
            methSym.argList.::(argSymbol)
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
          variable.setSymbol(membSymb)
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
            member.setSymbol(memSymb)
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
      klass.id.setSymbol(classSym)
      for (varDecl <- klass.vars) {
        /*
         * old Version was : setTypeSymbol(varDecl.tpe, gs)
         */
        setTypeSymbol(varDecl, classSym, gs)
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      val MethodSym = cs.lookupMethod(meth.id.value).get
      meth.setSymbol(MethodSym)
      meth.id.setSymbol(MethodSym)
      meth.args foreach (p => setISymbol(p.id)(Some(MethodSym)))
      meth.vars foreach (p => setISymbol(p.id)(Some(MethodSym)))
      meth.stats foreach (s => setSSymbols(s)(gs, Some(MethodSym)))
      setESymbols(meth.retExpr)(gs, Some(MethodSym))
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = stat match {
      case Block(stats: List[StatTree]) => stats foreach (setSSymbols(_))
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        setESymbols(expr)
        setSSymbols(thn)
        if (els isDefined) setSSymbols(els.get)
      case While(expr: ExprTree, stat: StatTree) =>
        setESymbols(expr)
        setSSymbols(stat)
      case Println(expr: ExprTree) => setESymbols(expr)
      case Assign(id: Identifier, expr: ExprTree) =>
        if (ms isDefined) {
          setISymbol(id)
        } else error("Assignment made on the main object at " + id.position)
        setESymbols(expr)
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        if (ms isDefined) {
          setISymbol(id)
        } else error("Assignment made on the main object at " + id.position)
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
        args foreach (setESymbols(_))
      case Variable(id: Identifier) => setISymbol(id)
      case th: This =>
        if (ms isDefined) th.setSymbol(ms.get.classSymbol)
        else error(s"Cannot call reference to 'this' at${expr.position} on the main object")
      case NewIntArray(size: ExprTree) => setESymbols(size)
      case New(tpe: Identifier) =>
        val newClass = gs.lookupClass(tpe.value)
        if (newClass isDefined) tpe.setSymbol(newClass get)
        else error(s"The class which is constructed at ${tpe.position} has not been declared.")
      case Not(expr: ExprTree) => setESymbols(expr)
      case _                   =>
    }

    /*
     * Old version was :
     * def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {???}
     */
    
    def setTypeSymbol(tpe: VarDecl, cs: ClassSymbol, gs: GlobalScope): Unit = {
      val varSym = cs.lookupVar(tpe.id.value)
      tpe.id.setSymbol(varSym get)
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
