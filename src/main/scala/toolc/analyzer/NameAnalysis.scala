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
          ctx.reporter.error(s"Class ${c.id.value} is already defined at position ${global.classes.get(className).get.position}")
        } else if (c.id == "Object") {
          ctx.reporter.error(s"the class at position ${c.position} cannot bear the name 'Object' !")

        } else {
          global.classes + ((className, new ClassSymbol(className).setPos(c)))
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
            correspClassSym.members ++ correspClassSym.parent.get.members
            correspClassSym.methods ++ correspClassSym.parent.get.methods
            
            //and analyzing/adding this class own's members and methods
            analyseMembers(correspClassSym, c.vars)
            for (method <- c.methods) {
              ???
            }

          } else {
            //case of no inherited class
            analyseMembers(correspClassSym, c.vars)
            
            for (method <- c.methods) {
              val lookupMeth = correspClassSym.lookupMethod(method.id.value)
              if(lookupMeth isDefined){
                ctx.reporter.error(s"Method at position ${method.position} defined twice, first time at ${lookupMeth.get.position}")
              }else{
                
                val methSym = new MethodSymbol(method.id.value, correspClassSym).setPos(method)
                for(args <- method.args){
                  if(methSym.params.contains(args.id.value)){
                    ctx.reporter.error(s"method's arguments are duplicated, first occurence found at ${methSym.params(args.id.value).position}")
                  }else{
                    val argSymbol = new VariableSymbol(args.id.value).setPos(args)
                    methSym.params + ((args.id.value, argSymbol)) 
                    methSym.argList.::(argSymbol)
                  }
                } 
              }
            }
          }
          correspClassSym.setVisited
        }
      }

      def analyseMembers(currClass: ClassSymbol, vars: List[VarDecl]) = for (variable <- vars) {
        val varName = variable.id.value
        if (currClass.members.contains(varName)) {
          ctx.reporter.error(s"variable at ${variable.position} is already defined at ${currClass.members.get(varName).get.position}")
        } else {
          currClass.members + ((varName, new VariableSymbol(varName).setPos(variable)))
        }
      }

      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      // TODO: Traverse within each definition of the program
      //       and attach symbols to Identifiers and "this"
      ???
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      ??? // TODO
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      ??? // TODO
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

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      ??? // TODO
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      ??? // TODO
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
