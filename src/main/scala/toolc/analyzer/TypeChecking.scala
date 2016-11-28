package toolc
package analyzer

import ast.Trees._

import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    /** Type checks statements and return expression of the method */
    def tcMethod(meth: MethodDecl): Unit = {
      tcExpr(meth.retExpr, meth.retType.getType)
      meth.stats foreach tcStat
    }

    /**
     * Checks that the expression is a subtype of the ones in expectedTps.
     * If it's not, prints an error message and returns the error type.
     * Also adds missing symbols to methods in MethodCalls
     */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
      expr match {

        //boolean expression
        case Equals(lhs, rhs) =>
          (lhs.getType, rhs.getType) match {
            case (TClass(_), TClass(_)) =>
              tcExpr(lhs, lhs.getType)
              tcExpr(rhs, rhs.getType)
            case (x, y) =>
              if (x equals y) {
                tcExpr(lhs, x)
                tcExpr(rhs, y)
              } else error("Type Error: Cannot test equality between types " + x.toString + " and " + y.toString, lhs)
            case _ => // TODO
          }
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Not(expr) =>
          tcExpr(expr, TBoolean)

        //Int operations
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Plus(lhs, rhs) =>
          tcExpr(lhs, TInt, TString)
          tcExpr(rhs, TInt, TString)
          
        case NewIntArray(size) =>
          tcExpr(size, TInt)
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          
        /*case th: This =>
        case New(id) =>*/

        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
          obj.getType match{
            case TClass(clSym) =>
              if(!clSym.methods.contains(meth.value)){
                error(s"Type error: object ${clSym.name} doesn't have a method called ${meth.value}", expr)
              }
              val method = clSym.methods(meth.value)
              if(args.size != method.argList.size) error(s"method call doesn't have the same numbers as the method declaration in ${method.position}", meth)
              for( el <- args zip method.argList){
                tcExpr(el._1, el._2.getType)
              }
            case _ => error("Type error: Trying to call a method on a expression which doesn't evaluate as an object type", expr)
          }
          
        case _ =>
      }

      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
      }

    }

    /** Invokes tcExpr as needed in the expressions of stat */
    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) => stats foreach tcStat
      case If(expr, thn, els) =>
        tcExpr(expr, TBoolean)
        tcStat(thn)
        els match{
          case Some(el) => tcStat(el)
          case None =>
        }
      case While(expr, stat) =>
        tcExpr(expr, TBoolean)
        tcStat(stat)
      case Println(expr) =>
        tcExpr(expr, TBoolean, TString, TInt)
      case Assign(id, expr) =>
        val typeToAssign = id.getSymbol.getType
        tcExpr(expr, typeToAssign)
      case ArrayAssign(id, index, expr) =>
        if(id.getType != TIntArray)
          error("Type error: Cannot make an array assignment on an variable that is not a array", expr)
        tcExpr(index, TInt)
        tcExpr(expr, TInt)
      case DoExpr(e) =>
        tcExpr(e, TObject, TInt, TIntArray, TBoolean, TString)
    }

    prog.main.stats foreach tcStat
    prog.classes foreach tcClass

    prog
  }
}
