package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import lexer.Tokens.DOT
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

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
  
  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = ptree match{
      case Node('Expression ::= List('AndExpr, 'OrExprOpt), List(and, oropt)) =>
        val lhs = constructExpr(and)
        constructOption(oropt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateOr(lhs, rhs)
        }
      
      case Node('AndExpr ::= List('EqExpr, 'AndExprOpt), List(eq, andopt)) =>
        val lhs = constructExpr(eq)
        constructOption(andopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateAnd(lhs, rhs)
        }
        
      case Node('EqExpr ::= List('LtExpr, 'EqExprOpt), List(lt, eqopt)) =>
        val lhs = constructExpr(lt)
        constructOption(eqopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateEquals(lhs, rhs)
        }
        
      case Node('LtExpr ::= List('MinusExpr, 'LtExprOpt), List(minus, ltopt)) =>
        val lhs = constructExpr(minus)
        constructOption(ltopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateLessThan(lhs, rhs)
        }
        
      case Node('MinusExpr ::= List('PlusExpr, 'MinusExprOpt), List(plus, minusopt)) =>
        val lhs = constructExpr(plus)
        constructOption(minusopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateMinus(lhs, rhs)
        }
        
      case Node('PlusExpr ::= List('DivExpr, 'PlusExprOpt), List(div, plusopt)) =>
        val lhs = constructExpr(div)
        constructOption(plusopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociatePlus(lhs, rhs)
        }
        
      case Node('DivExpr ::= List('MultExpr, 'DivExprOpt), List(mult, divopt)) =>
        val lhs = constructExpr(mult)
        constructOption(divopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateDiv(lhs, rhs)
        }
      
      case Node('MultExpr ::= List('BangExpr, 'MultExprOpt), List(bang, multopt)) =>
        val lhs = constructExpr(bang)
        constructOption(multopt, constructExpr) match {
          case None => lhs.setPos(lhs)
          case Some(rhs) => leftAssociateTimes(lhs, rhs)
        }
      
      case Node('BangExpr ::= List(BANG(),'ArrayExpr), List(Leaf(bt), e)) =>
        Not(constructExpr(e)).setPos(bt)
        
      case Node('BangExpr ::= List('ArrayExpr), List(e)) => constructExpr(e)
      
      case Node('ArrayExpr ::= _, List(dot, arrayopt))=>
        val dotExpr = constructExpr(dot)
        //diff
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
      
      //this case accounts for all the (Op)ExprOpt of the grammar
      case Node(_, List(_, expr)) => 
        val pe = constructExpr(expr)
        pe.setPos(pe)
  }
  
  //need to do it left recursive style too.
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
  
  def leftAssociateOr(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Or(l, r) => leftAssociateOr(Or(lhs, l).setPos(lhs), r).setPos(Or(lhs, l))
          case _  => Or(lhs, rhs).setPos(lhs)
  }
  
  def leftAssociateAnd(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case And(l, r) => leftAssociateAnd(And(lhs, l).setPos(lhs), r).setPos(And(lhs, l))
          case _  => And(lhs, rhs).setPos(lhs)
  }
  
  def leftAssociateEquals(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Equals(l, r) => leftAssociateEquals(Equals(lhs, l).setPos(lhs), r).setPos(Equals(lhs, l))
          case _  => Equals(lhs, rhs).setPos(lhs)
  }
  
  def leftAssociateLessThan(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case LessThan(l, r) => leftAssociateLessThan(LessThan(lhs, l).setPos(lhs), r).setPos(LessThan(lhs, l))
          case _  => LessThan(lhs, rhs).setPos(lhs)
  }
  
  def leftAssociateMinus(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Minus(l, r) => leftAssociateMinus(Minus(lhs, l).setPos(lhs), r).setPos(Minus(lhs, l))
          case _  => Minus(lhs, rhs).setPos(lhs)
  }

  def leftAssociatePlus(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Plus(l, r) => leftAssociatePlus(Plus(lhs, l).setPos(lhs), r).setPos(Plus(lhs, l))
          case _  => Plus(lhs, rhs).setPos(lhs)
  }


  def leftAssociateDiv(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Div(l, r) => leftAssociateDiv(Div(lhs, l).setPos(lhs), r).setPos(Div(lhs, l))
          case _  => Div(lhs, rhs).setPos(lhs)
  }

  
  def leftAssociateTimes(lhs: ExprTree, rhs: ExprTree): ExprTree = rhs match{
          case Times(l, r) => leftAssociateTimes(Times(lhs, l).setPos(lhs), r).setPos(Times(lhs, l))
          case _  => Times(lhs, rhs).setPos(lhs)
  }
  
  
  
}
