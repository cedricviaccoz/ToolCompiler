package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import GrammarUtils.InLL1
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(), 
    // FIRST = {PROGRAM()}
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(), //FIRST = {PROGRAM()}
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    // FIRST = {LBRACE, WHILE, PRINTLN, IDSENT, DO}
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(), //FIRST = {EQSIGN, LBRACKET}
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression // LEFT RECURSION HERE !
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | BANG() ~ 'Expression
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    //FIRST = {e, COMMA()}
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  // TODO: Transform this to an LL(1) grammar
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
      //no left-recursion anymore
      //there seems to be nothing to left-fatorize
      // the first rule of LL(1) (A -> a|b, First(a) (intersection) First(b) = 0
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
      //FIRST = program
      //FOLLOW = 
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
      //FIRST = program
      //FOLLOW = class
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
      //FIRST = e if { while println IDSENT DO
      //FOLLOW = }
      //NULLABLE
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
      //FIRST = e class
      //FOLLOW = EOF 
      //NULLABLE
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
      //FIRST = class
      //FOLLOW = class EOF
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
      //FIRST = e extends
      //FOLLOW = {
      //NULLABLE
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
      //FIRST = {
      //FOLLOW = 
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
      //FIRST = e var
      //FOLLOW = 
      //NULLABLE
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
      //FIRST = var
      //FOLLOW = 
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
      //FIRST = e def
      //FOLLOW = 
      //NULLABLE
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
      //FIRST = def
      //FOLLOW = 
    'Params ::= epsilon() | 'Param ~ 'ParamList,
      //FIRST = e IDSENT
      //FOLLOW = 
      //NULLABLE
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
      //FIRST = e ,
      //FOLLOW =
      //NULLABLE
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
      //FIRST = IDSENT
      //FOLLOW = 
    'Type ::= INT() ~ 'ArrayDeclaration
      | BOOLEAN()  
      | STRING() 
      | 'Identifier,
      //FIRST = Int Boolean String IDSENT
      //FOLLOW = 
    'ArrayDeclaration ::= epsilon()| LBRACKET() ~ RBRACKET(),
      //FIRST = e [ 
      //FOLLOW = 
      //NULLABLE
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
      //FIRST = if { while println IDSENT DO
      //FOLLOW = 
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
      // FIRST = if { while println IDSENT DO
      //FOLLOW = 
    'SimpleStat ::= LBRACE() ~ 'Stmt  ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
      //FIRST = { while println IDSENT DO
      //FOLLOW = 
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
      //FIRST = = [
      //FOLLOW = 
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
      //FIRST = else
      //FOLLOW = 
      //NULLABLE
    'Expression ::= INTLITSENT ~ 'ExpressionAlt
      | STRINGLITSENT ~ 'ExpressionAlt
      | TRUE() ~ 'ExpressionAlt
      | FALSE() ~ 'ExpressionAlt
      | 'Identifier ~ 'ExpressionAlt
      | THIS() ~ 'ExpressionAlt
      | NEW() ~ 'newExpr ~ 'ExpressionAlt
      | BANG() ~ 'Expression ~ 'ExpressionAlt
      | LPAREN() ~ 'Expression ~ RPAREN() ~ 'ExpressionAlt,
      //FIRST = INTLIT STRINGLIT true false this new ! (
      //FOLLOW = 
    'ExpressionAlt ::= 'Op ~ 'Expression ~ 'ExpressionAlt
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionAlt
      | DOT() ~ LENGTH() ~ 'ExpressionAlt
      | DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionAlt 
      | epsilon(),
      //FIRST = e { . && || == < + - * /
      //FOLLOW = 
      //NULLABLE
    'newExpre ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Identifier ~ LPAREN() ~ RPAREN(),
      //FIRST = Int IDSENT
      //FOLLOW = 
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
      //FIRST = e INTLIT STRINGLIT true false this new ! (
      //FOLLOW = 
      //NULLABLE
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
      //FIRST = e ,
      //FOLLOW = 
      //NULLABLE
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
      // FIRST = && || == < + - * /
      //FOLLOW = 
    'Identifier ::= IDSENT
      //FIRST = IDSENT
      //FOLLOW = { extends
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, list)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }
  }

}
