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
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Stmt ~ 'Stmts | epsilon(),
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
    'Stmt ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Stmt | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
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
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))
      
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Stmt ~ 'Stmts 
      | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls 
      | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= EXTENDS() ~ 'Identifier 
      | epsilon(),
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs 
      | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs 
      | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= 'Param ~ 'ParamList 
      | epsilon(),
    'ParamList ::= COMMA() ~ 'Param ~ 'ParamList
      | epsilon(),
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ 'ArrayDeclaration
      | BOOLEAN()  
      | STRING() 
      | 'Identifier,
    'ArrayDeclaration ::= LBRACKET() ~ RBRACKET()
      | epsilon(),
    'Stmt ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts   ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(), 
    'ElseOpt ::= ELSE() ~ 'Stmt 
      | epsilon(),
    'Expression ::= 'OrExpr,
    'OrExpr ::= 'AndExpr ~ 'OrExprOpt,
    'OrExprOpt ::= OR() ~ 'OrExpr | epsilon(),
    'AndExpr ::= 'EqExpr ~ 'AndExprOpt,
    'AndExprOpt ::= AND() ~ 'AndExpr | epsilon(),
    'EqExpr ::= 'LtExpr ~ 'EqExprOpt,
    'EqExprOpt ::= EQUALS() ~ 'EqExpr | epsilon(),
    'LtExpr ::= 'MinusExpr ~ 'LtExprOpt,
    'LtExprOpt ::= LESSTHAN() ~ 'LtExpr | epsilon(),
    'MinusExpr ::= 'PlusExpr ~ 'MinusExprOpt,
    'MinusExprOpt ::= MINUS() ~ 'MinusExpr | epsilon(),
    'PlusExpr ::= 'DivExpr ~ 'PlusExprOpt,
    'PlusExprOpt ::= PLUS() ~ 'PlusExpr | epsilon(),
    'DivExpr ::= 'MultExpr ~ 'DivExprOpt,
    'DivExprOpt ::= DIV() ~ 'DivExpr | epsilon(),
    'MultExpr ::= 'BangExpr ~ 'MultExprOpt,
    'MultExprOpt ::= TIMES() ~ 'MultExpr | epsilon(),
    'BangExpr ::= BANG() ~ 'ArrayExpr | 'ArrayExpr,
    'ArrayExpr ::= LBRACKET() ~ 'ExpressionOpt ~ RBRACKET() | 'DotExpr,
    'ExpressionOpt ::= 'Expression | epsilon(),
    'DotExpr ::= 'NewExpr ~ 'DotExprOpt,
    'DotExprOpt ::= DOT() ~ 'MethOrLength ~ 'DotExprOpt | epsilon(),
    'MethOrLength ::= LENGTH() | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN(),
    'NewExpr ::= NEW() ~ 'IntArrayOrId | 'termExpr ~ 'brackBraceOpt,
    'IntArrayOrId ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET() | 'Identifier ~ LPAREN() ~ RPAREN(),
    'brackBraceOpt ::= LPAREN() ~ 'Expression ~ RPAREN() | LBRACKET() ~ 'Expression ~ RBRACKET() | epsilon(), // to treat E(E) and E[E]
    'termExpr ::= LPAREN()~ 'ExpressionOpt ~ RPAREN()
      | TRUE() | FALSE() | 'Identifier | STRINGLITSENT | INTLITSENT | THIS(),
    'Args ::= 'Expression ~ 'ExprList
      | epsilon(),
    'ExprList ::= COMMA() ~ 'Expression ~ 'ExprList
      | epsilon(),
    'Identifier ::= IDSENT
  ))
      
  
  
  val toolOpPrecedenceLL1 = Grammar('Program, List[Rules[Token]](
    'Expression ::= 'OrExpr,
    'OrExpr ::= 'AndExpr ~ 'OrExprOpt,
    'OrExprOpt ::= OR() ~ 'OrExpr | epsilon(),
    'AndExpr ::= 'EqExpr ~ 'AndExprOpt,
    'AndExprOpt ::= AND() ~ 'AndExpr | epsilon(),
    'EqExpr ::= 'LtExpr ~ 'EqExprOpt,
    'EqExprOpt ::= EQSIGN() ~ 'EqExpr | epsilon(),
    'LtExpr ::= 'MinusExpr ~ 'LtExprOpt,
    'LtExprOpt ::= LESSTHAN() ~ 'LtExpr | epsilon(),
    'MinusExpr ::= 'PlusExpr ~ 'MinusExprOpt,
    'MinusExprOpt ::= MINUS() ~ 'MinusExpr | epsilon(),
    'PlusExpr ::= 'DivExpr ~ 'PlusExprOpt,
    'PlusExprOpt ::= PLUS() ~ 'PlusExpr | epsilon(),
    'DivExpr ::= 'MultExpr ~ 'DivExprOpt,
    'DivExprOpt ::= DIV() ~ 'DivExpr | epsilon(),
    'MultExpr ::= 'BangExpr ~ 'MultExprOpt,
    'MultExprOpt ::= TIMES() ~ 'MultExpr | epsilon(),
    //now it is getting a little bit tricky, will need tests and careful thinking
    'BangExpr ::= BANG() ~ 'ArrayExpr | 'ArrayExpr,
    'ArrayExpr ::= LBRACKET() ~ 'ExpressionOpt ~ RBRACKET() | 'DotExpr,
    'ExpressionOpt ::= 'Expression | epsilon(),
    'DotExpr ::= 'NewExpr ~ 'DotExprOpt,
    'DotExprOpt ::= DOT() ~ 'MethOrLength | epsilon(),
    'MethOrLength ::= LENGTH() | 'Identifier,
    'NewExpr ::= NEW() ~ 'IntArrayOrId | 'termExpr,
    'IntArrayOrId ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET() | 'Identifier,
    'termExpr ::= LPAREN()~ 'ExpressionOpt ~ RPAREN()
      | TRUE() | FALSE() | IDSENT | STRINGLITSENT | INTLITSENT | THIS()
      ))
      
      
  val Oldll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Stmt ~ 'Stmts 
      | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls 
      | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= EXTENDS() ~ 'Identifier 
      | epsilon(),
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs 
      | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs 
      | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= 'Param ~ 'ParamList 
      | epsilon(),
    'ParamList ::= COMMA() ~ 'Param ~ 'ParamList
      | epsilon(),
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ 'ArrayDeclaration
      | BOOLEAN()  
      | STRING() 
      | 'Identifier,
    'ArrayDeclaration ::= LBRACKET() ~ RBRACKET()
      | epsilon(),
    'Stmt ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmt  ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(), 
    'ElseOpt ::= ELSE() ~ 'Stmt 
      | epsilon(),
    'Expression ::= INTLITSENT ~ 'ExpressionAlt
      | STRINGLITSENT ~ 'ExpressionAlt
      | TRUE() ~ 'ExpressionAlt
      | FALSE() ~ 'ExpressionAlt
      | 'Identifier ~ 'ExpressionAlt
      | THIS() ~ 'ExpressionAlt
      | NEW() ~ 'newExpr ~ 'ExpressionAlt
      | BANG() ~ 'Expression ~ 'ExpressionAlt
      | LPAREN() ~ 'Expression ~ RPAREN() ~ 'ExpressionAlt,
    'ExpressionAlt ::= 'Op ~ 'Expression ~ 'ExpressionAlt
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionAlt
      | DOT() ~ LENGTH() ~ 'ExpressionAlt
      | DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionAlt 
      | epsilon(),
    'newExpr ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Identifier ~ LPAREN() ~ RPAREN(),
    'Args ::= 'Expression ~ 'ExprList
      | epsilon(),
    'ExprList ::= COMMA() ~ 'Expression ~ 'ExprList
      | epsilon(),
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
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
