package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import com.sun.org.apache.xalan.internal.xsltc.compiler.Whitespace

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case ":" => Some(COLON())
    case ";" => Some(SEMICOLON())
    case "." => Some(DOT())
    case "," => Some(COMMA())
    case "=" => Some(EQSIGN())
    case "=="=> Some(EQUALS())
    case "!" => Some(BANG())
    case "(" => Some(LPAREN())
    case ")" => Some(RPAREN())
    case "[" => Some(LBRACKET())
    case "]" => Some(RBRACKET())
    case "{" => Some(LBRACE())
    case "}" => Some(RBRACE())
    case "&&" => Some(AND())
    case "||" => Some(OR())
    case "<" => Some(LESSTHAN())
    case "+" => Some(PLUS())
    case "-" => Some(MINUS())
    case "*" => Some(TIMES())
    case "/" => Some(DIV())
    case "program" => Some(PROGRAM())
    case "class" => Some(CLASS())
    case "def" => Some(DEF())
    case "var" => Some(VAR())
    case "String" => Some(STRING())
    case "extends" => Some(EXTENDS())
    case "Int" => Some(INT())
    case "Bool" => Some(BOOLEAN())
    case "if" => Some(IF())
    case "else" => Some(ELSE())
    case "while" => Some(WHILE())
    case "return" => Some(RETURN())
    case "length" => Some(LENGTH())
    case "true" => Some(TRUE())
    case "false" => Some(FALSE())
    case "this" => Some(THIS())
    case "new" => Some(NEW())
    case "println" => Some(PRINTLN())
    case "do" => Some(DO())
    case _          => None
  }
  
  private def isASpecialChar(c: Char): Boolean = c match{
    case ':' => true
    case ';' => true
    case '.' => true
    case ',' => true
    case '=' => true
    case '!' => true
    case '(' => true
    case ')' => true
    case '[' => true
    case ']' => true
    case '{' => true
    case '}' => true
    case '<' => true
    case '+' => true
    case '-' => true
    case '*' => true
    case '/' => true
    case '?' => true
    case '^' => true
    case '#' => true
    case '|' => true
    case '&' => true
    case '@' => true
    case '~' => true
    case '¬' => true
    case '>' => true
    case '§' => true
    case '°' => true
    case '½' => true
    case '%' => true
    case '¢' => true
    case '´' => true
    case '`' => true
    case '$' => true
    case '£' => true
    case '¨' => true
    case _ => false
  }
  
  //that's a very lazy way to treat such case but so is life.
  private def thoseExceptions(c1: Char, c2: Char): Boolean = {
    var s = c1.toString() + c2.toString()
    s match{
      case "((" => true
      case "))" => true
      case ";;" => true
      case "{{" => true
      case "}}" => true
      case "]]" => true
      case "[[" => true
      case "!!" => true
      case _ => false
    }
    
  }  
  private def isDoubledChar(c1: Char, c2: Char): Boolean = c1 == c2


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      while (Character.isWhitespace(currentChar)) {
        consume()
      }
      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        // Skip until EOL
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        //The position at the beginning of the current comment to report in case of failing.
        val posOfComment = currentPos 
        consume(2)
        while(!((currentChar == '*' && nextChar == '/') || (currentChar == EndOfFile))){
          consume()
        }
        if(currentChar == EndOfFile){
          error("the comment block did never close !", posOfComment)
        }
        consume(2)
        nextToken()
      } else {
        readToken()
      }
    }
    
    lazy val listOfDigits: List[Char] = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    lazy val listOfAlphaDigits: List[Char] = List('0', '1', '2', '3', '4', '5', '6', '7', '8', 
         '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 
         'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z','_')
         
    /** Reads the next token from the stream. */
    //@scala.annotation.tailrec
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos: Positioned = currentPos
      
      if(currentChar == '"'){
        
        consume() //to avoid putting the " in the String.
        var str: String = ""
        while(currentChar != '"' && currentChar != EndOfFile){
          if(currentChar == '\n' || currentChar == '\r'){
            error("there should not be back to the line in String Literals", currentPos)
            consume()
          }else{
           str += currentChar
           consume() 
          }
        }
        if(currentChar == EndOfFile){
          error("String Literal not closed", tokenPos)
        }
        consume() // for currentChar not to be == '"' after that.
        STRINGLIT(str).setPos(tokenPos)
        
      }
      else if(isDoubledChar(currentChar, nextChar) 
              && isASpecialChar(currentChar) 
              && !thoseExceptions(currentChar, nextChar)){
        
        //testing the case of token such as "&&", "||" and "=="
        val s: String = currentChar.toString() + nextChar.toString()
        consume(2)
        tokenAfterKW(s, tokenPos)
        
      }
      else if(isASpecialChar(currentChar)){
        
        //case when we encoutered a special character (
        val previousChar: String = currentChar.toString()
        consume()
        tokenAfterKW(previousChar, tokenPos) 
        
      }else if(currentChar == EndOfFile)
        EOF().setPos(tokenPos)
      else{
        /*now trying to iterate to find a token that is either a keyword, 
         * an identifier, or a IntLitteral.
         */
        var strBuild: String = ""
        while(!Character.isWhitespace(currentChar) && !isASpecialChar(currentChar) && currentChar != EndOfFile){
          strBuild += currentChar
          consume()
        }
        val tokToTest: String = strBuild
        keywords(tokToTest) match {
          case Some(token) => token.setPos(tokenPos)
          case None => {
            //now we need to test wether it's and identifier, or a Intliteral
            if(tokToTest != "" && tokToTest.forall { c => listOfDigits.contains(c)}){
              INTLIT(tokToTest.toInt).setPos(tokenPos)
            }else if(tokToTest != "" && tokToTest.forall{c => listOfAlphaDigits.contains(c.toLower)}) {
              if(tokToTest(0) == '_'){
                error("an identifier should not start with '_'", tokenPos)
                BAD().setPos(tokenPos)
              }else if(listOfDigits.contains(tokToTest(0))){
                error("an identifier should always start with a letter", tokenPos)
                BAD().setPos(tokenPos)
                
              }else{
                ID(tokToTest).setPos(tokenPos) 
              }
            }else{
              error("not conform character written", currentPos)
              BAD().setPos(tokenPos)
            }
          }
        }
      }
    }
    
    //just for modularity.
    def tokenAfterKW(s: String, tokenPos: Positioned): Token = {
      keywords(s) match {
          case Some(token) => token.setPos(tokenPos)
          case None => error("not conform character written,", tokenPos);BAD().setPos(tokenPos)
      }
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}
