package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import lexer.Tokens.DOT
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  //override since I made a new rule for Int and Int[]
  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= INT() :: _, List(Leaf(i@INT()), arrayDecl)) =>
        arrayDecl match {
          case Node(_, List()) => IntType().setPos(i)
          case Node(_, List(Leaf(lb), Leaf(rb))) => IntArrayType().setPos(i)
        }
      case Node('Type ::= _, List(Leaf(b@BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s@STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }
  
  //override according to the redefinition of my newly constructed rules.
  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = ptree match{
    
      
      case Node('BangExpr ::= List(BANG(),'ArrayExpr), List(Leaf(bt), e)) =>
        Not(constructExpr(e)).setPos(bt)
    
      case Node('BangExpr ::= List('ArrayExpr), List(e)) => constructExpr(e)
      
      case Node('ArrayExpr ::= _, List(dot, arrayopt))=>
        val dotExpr = constructExpr(dot)
        arrayopt match{
          case Node(_, List()) => dotExpr.setPos(dotExpr)
          case Node(_, List(Leaf(lb), idx,_ )) =>
            val index = constructExpr(idx)
            ArrayRead(dotExpr, index).setPos(dotExpr)
        }
        
      case Node('DotExpr ::=_, List(nexpr, dotopt)) => 
        val newExp = constructExpr(nexpr)
        dotopt match{
          case Node(_, List()) => newExp.setPos(newExp)
          case Node(_, List(Leaf(dt@DOT()), methOrlen)) => handleDotOptRightRecursion(methOrlen, newExp, dt)
        }
        
      case Node('NewExpr ::= List('termExpr), List(terminal)) => evaluateTerminal(terminal) 
      
      case Node('NewExpr ::= NEW() :: List('IntArrayOrId) , List(Leaf(nt@NEW()), iaoi)) =>
        iaoi match{
          case Node(_, List(_,_, expr,_)) => NewIntArray(constructExpr(expr)).setPos(nt)
          case Node(_, List(id,_,_)) => New(constructId(id)).setPos(nt)
        } 
        
      //treating all binary operations case
      case Node(_, List(expr, expropt)) =>
        val lhs = constructExpr(expr)
        leftAssociate(lhs.setPos(lhs), expropt)
  }
  
  def handleDotOptRightRecursion(dotexpropt: NodeOrLeaf[Token], 
                                 obj: ExprTree,
                                 position: DOT): ExprTree = dotexpropt match{
    case Node(_, List(id,_,args,_,opt)) => 
      val meth = 
        MethodCall(obj, constructId(id), constructList(args, constructExpr, hasComma = true)).setPos(position)
      opt match{
        case Node(_, List()) => meth
        case Node(_, List(Leaf(dt@DOT()), methOrlen)) => 
          handleDotOptRightRecursion(methOrlen, meth, dt)
      }
    case Node(_, List(_)) => 
      ArrayLength(obj).setPos(position)
  }
  
  def evaluateTerminal(terminal: NodeOrLeaf[Token]): ExprTree = terminal match{
    case Node('termExpr::= _, List(Leaf(lp),expr,_)) => constructExpr(expr).setPos(lp) 
    case Node(_, List(Leaf(tt@TRUE()))) => True().setPos(tt)
    case Node(_, List(Leaf(ft@FALSE()))) => False().setPos(ft)
    case Node(_, List(Leaf(tht@THIS()))) => This().setPos(tht)
    case Node(_, List(Leaf(it@INTLIT(i)))) => IntLit(i).setPos(it)
    case Node(_, List(Leaf(st@STRINGLIT(s)))) => StringLit(s).setPos(st)
    case Node(_, List(id)) => 
        val pid = constructId(id)
        Variable(pid).setPos(pid) 
  }
  
  
  //take care of left associativity with all the binary operators of the grammar.
  def leftAssociate(lhs : ExprTree, rule: NodeOrLeaf[Token]): ExprTree = rule match {
    case Node('OrExprOpt ::= _, List(Leaf(ot@OR()), orexp )) =>
      orexp match{
        case Node('Expression::= _, List(andexp, orop)) =>
          val l = constructExpr(andexp)
          leftAssociate(Or(lhs, l).setPos(ot), orop)
        case _ => Or(lhs, constructExpr(orexp)).setPos(ot)
      }
      
    case Node('AndExprOpt ::= _, List(Leaf(at@AND()), andexp )) =>
      andexp match{
        case Node('AndExpr::= _, List(cmpexp, andop)) =>
          val l = constructExpr(cmpexp)
          leftAssociate(And(lhs, l).setPos(at), andop)
        case _ => And(lhs, constructExpr(andexp)).setPos(at)
      }
      
      
    case Node('CmpExprOpt ::= _, List(Leaf(et@EQUALS()), cmpexp )) =>
      cmpexp match{
        case Node('CmpExpr::= _, List(asexp, cmpop)) =>
          val l = constructExpr(asexp)
          leftAssociate(Equals(lhs, l).setPos(et), cmpop)
        case _ => Equals(lhs, constructExpr(cmpexp)).setPos(et)
      }
      
    case Node('CmpExprOpt ::= _, List(Leaf(ltt@LESSTHAN()), cmpexp )) =>
      cmpexp match{
        case Node('CmpExpr::= _, List(asexp, cmpop)) =>
          val l = constructExpr(asexp)
          leftAssociate(LessThan(lhs, l).setPos(ltt), cmpop)
        case _ => LessThan(lhs, constructExpr(cmpexp)).setPos(ltt)
      }
      
    case Node('AddSubExprOpt ::= _, List(Leaf(mt@MINUS()), addsubexp )) =>
      addsubexp match{
        case Node('AddSubExpr::= _, List(factexp, addsubop)) =>
          val l = constructExpr(factexp)
          leftAssociate(Minus(lhs, l).setPos(mt), addsubop)
        case _ => Minus(lhs, constructExpr(addsubexp)).setPos(mt)
      }
     
    case Node('AddSubExprOpt ::= _, List(Leaf(pt@PLUS()), addsubexp )) =>
      addsubexp match{
        case Node('AddSubExpr::= _, List(factexp, addsubop)) =>
          val l = constructExpr(factexp)
          leftAssociate(Plus(lhs, l).setPos(pt), addsubop)
        case _ => Plus(lhs, constructExpr(addsubexp)).setPos(pt)
      }
    
    case Node('FactorExprOpt ::= _, List(Leaf(dt@DIV()), factexp )) =>
      factexp match{
        case Node('FactorExpr::= _, List(bangexp, factop)) =>
          val l = constructExpr(bangexp)
          leftAssociate(Div(lhs, l).setPos(dt), factop)
        case _ => Div(lhs, constructExpr(factexp)).setPos(dt)
      }
      
    case Node('FactorExprOpt ::= _, List(Leaf(tt@TIMES()), factexp )) =>
      factexp match{
        case Node('FactorExpr::= _, List(bangexp, factop)) =>
          val l = constructExpr(bangexp)
          leftAssociate(Times(lhs, l).setPos(tt), factop)
        case _ => Times(lhs, constructExpr(factexp)).setPos(tt)
      }
      
      case _ => lhs
  }
}
