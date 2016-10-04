package toolc
package eval

import ast.Trees._
import utils._
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    //not totally sure about this one, I mainly copied what is coded with "eval()"
    case Block(stats) => stats.foreach(evalStatement(_))
    
    case If(expr, thn, els) => 
      if(evalExpr(expr).asBool) evalStatement(thn)
      else els match{case Some(code) => evalStatement(code); case None =>}
    
    case While(expr, stat) =>
      while(evalExpr(expr).asBool){
        evalStatement(stat)
      }
      
    case Println(expr) => evalExpr(expr) match{
      case StringValue(str) => println(str)
      case IntValue(int) => println(int)
      case BoolValue(bool) => println(bool)
      case _ => fatal("println can only be used on Strings, Int and Bools.")
    }
    
    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(expr))
    
    case ArrayAssign(id, index, expr) => 
      ectx.getVariable(id.value).asArray.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)
      
    //not sure if declaring a "val" isn't considered a return value in the mad-world of Scala.
    case DoExpr(expr) => val void = evalExpr(expr) 
    
    //it would be great if i had a way to do this.
    //case VarDecl(tpe, id) => ectx.declareVariable(id.value)
  }
  

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = {
    //TODO you need to treat the case var b: Bool. with ectx.declareVariable
    
      //to avoid code repetition in the basic arithmetic evaluation.    
      def arithmeticIntHelper(lhs: ExprTree, rhs: ExprTree, msg: String, operator: (Int, Int) => Int): Value = 
        (evalExpr(lhs), evalExpr(rhs)) match{
          case (int1: IntValue, int2: IntValue) => IntValue(operator(int1.asInt, int2.asInt))
          case _ => fatal(msg+" only defined for Int type")
        }
        
      e match {
          case IntLit(value) => IntValue(value)
          case StringLit(value) => StringValue(value)
          case True() => BoolValue(true)
          case False() => BoolValue(false)
          
          //scala already makes some short-circuiting itself, so no need to implement it explicitly here for tool.
          case And(lhs, rhs) => BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool) 
          case Or(lhs, rhs)  => BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)
          
          case Plus(lhs, rhs) => 
            val l = evalExpr(lhs)
            val r = evalExpr(rhs)
            (l, r) match{
              case (int1: IntValue, int2: IntValue) => IntValue(int1.asInt + int2.asInt)
              case (int: IntValue, str: StringValue) => StringValue(int.asInt + str.asString)
              case (str: StringValue, int: IntValue) => StringValue(str.asString + int.asInt)
              case (str1: StringValue, str2: StringValue) => StringValue(str1.asString + str2.asString)
              case _ => fatal("Type Error for addition")
            }
            
          case Minus(lhs, rhs) => arithmeticIntHelper(lhs, rhs, "substraction", _-_)
            
          case Times(lhs, rhs) => arithmeticIntHelper(lhs, rhs, "multiplication", _*_)
          
          case Div(lhs, rhs) => arithmeticIntHelper(lhs, rhs, "substraction", _/_)
          
          case LessThan(lhs, rhs) => (evalExpr(lhs), evalExpr(rhs)) match{
              case (int1: IntValue, int2: IntValue) => BoolValue(int1.asInt < int2.asInt)
              case _ => fatal("< only defined for Int type")
          }
          
          case Not(expr) => BoolValue(!evalExpr(expr).asBool)
          
          case Equals(lhs, rhs) => 
             val l = evalExpr(lhs)
             val r = evalExpr(rhs)
             (l,r) match {
               case (IntValue(int1), IntValue(int2)) => BoolValue(int1 == int2)
               case (BoolValue(bool1), BoolValue(bool2)) => BoolValue(bool1 == bool2)
               case (_: ArrayValue, _: ArrayValue) => BoolValue(l eq r)
               case (_: ObjectValue, _: ObjectValue) => BoolValue(l eq r)
               case (_: StringValue, _: StringValue) => BoolValue(l eq r)
               case _ => fatal("Type Error")
             }
             
          case ArrayRead(arr, index) => IntValue(evalExpr(arr).asArray.getIndex(evalExpr(index).asInt))
          
          case ArrayLength(arr) => IntValue(evalExpr(arr).asArray.length)
          
          case MethodCall(obj, meth, args) =>
            val currObject: ObjectValue = evalExpr(obj).asObject
            val currClass: ClassDecl = currObject.cd
            val currMethod: MethodDecl = findMethod(currClass, meth.value)
            val funcContext = new MethodContext(currObject)
            /*
             * Now we need to associate the evaluation of the argument to
             * the "fields" of the method. then we will finally be able
             * to run the code of the m|ethod. 
             * Hell.
             * Apparently the method declaration got everything you need bro.
             */
            val evaluatedArgs: List[Value] = args.map{evalExpr(_)}
            val methodArgs : List[Formal] = currMethod.args
            
            //adding the name of the args to the funcContext
            methodArgs foreach (formal => funcContext.declareVariable(formal.id.value))
            
            //bindings the arguments of the method with the computed values.
            val varPlusValue: List[(Formal, Value)] = methodArgs zip evaluatedArgs //make a list of tuple
            for((a, v) <- varPlusValue){
              funcContext.setVariable(a.id.value, v)
            }
            
            //adding the vars of the method declaration to the funcContext
            currMethod.vars foreach (v => funcContext.declareVariable(v.id.value))
            
            //evaluation of the statements of the method.
            currMethod.stats.foreach(evalStatement(_)(funcContext))
            
            //evaluation of the return expression.
            evalExpr(currMethod.retExpr)(funcContext)      
            
            //then it's done. yEAH.
            
          case Variable(Identifier(name)) => ectx.getVariable(name)
          
          case New(tpe) => {
            //we need to initalise all the fields of the class in the current instantiated object.
            val currClass: ClassDecl = findClass(tpe.value)
            val currObj: ObjectValue = ObjectValue(currClass)
            fieldsOfClass(currClass) foreach (str => currObj.declareField(str))
            return currObj
          }
          case This() => ectx match{
            case _: MainContext => fatal("can't reach 'this' on the MainContext")
            case ctx : MethodContext => ctx.obj
          }
          case NewIntArray(size) => ArrayValue(new Array[Int](evalExpr(size).asInt))
    }
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value          = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit       = unavailable
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }
  
  /*
   * for when you got 
   * class A{
   *  var a;
   *  var b;
   * }
   */
  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int            = expected("Int")
    def asString: String      = expected("String")
    def asBool: Boolean       = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue   = expected("Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length-1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
