package net.foggin.rules.scala

import Character._

abstract class ScalaToken

abstract class ReservedId extends ScalaToken {
  def id : String
  def canStartStatement = !ReservedId.cannotStartStatement.contains(id)
  def canTerminateStatement = !ReservedId.terminateStatement.contains(id)
}

object ReservedId {
  val reserved = _root_.scala.collection.immutable.Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")

  /** Reserved ids that can terminate a statement */
  val terminateStatement = _root_.scala.collection.immutable.Set(
      "this", "null", "true", "false", "return", "type", "_")
  
  /** Reserved ids that cannot start a statement */
  val cannotStartStatement = _root_.scala.collection.immutable.Set(
      "catch", "else", "extends", "finally", "forSome", "match", "requires", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "\u21D2")

  def apply(id : String) = isReserved(id)
  def isReserved(id : String) = reserved.contains(id)
}

case class Keyword(id : String) extends ReservedId
case class ReservedOperator(id : String) extends ReservedId

case object Newline extends ScalaToken
case object Semicolon extends ScalaToken
case object Dot extends ScalaToken
case object Comma extends ScalaToken
case object OpenRound extends ScalaToken
case object CloseRound extends ScalaToken
case object OpenSquare extends ScalaToken
case object CloseSquare extends ScalaToken
case object OpenCurly extends ScalaToken
case object CloseCurly extends ScalaToken

abstract class IdToken extends ScalaToken { def id : String }

case class Operator(id : String) extends IdToken
case class QuotedId(id : String) extends IdToken
case class UnquotedId(varId : Boolean, id : String) extends IdToken

abstract class Literal extends ScalaToken with Expression

case object Null extends Literal
case object True extends Literal
case object False extends Literal

case class CharacterLiteral(char : Char) extends Literal
case class StringLiteral(string : String) extends Literal
case class SymbolLiteral(symbol : Symbol) extends Literal
case class IntegerLiteral(value : Int) extends Literal
case class LongLiteral(value : Long) extends Literal
case class FloatLiteral(value : Float) extends Literal
case class DoubleLiteral(value : Double) extends Literal

case object Comment extends ScalaToken
case object Space extends ScalaToken


/** Defines the Scala lexical syntax rules.
  *
  * Note: Embedded XML not yet implemented.
  */
abstract class ScalaScanner extends Scanner {
  
  /**
   * Input for Scala Parser.
   */
  class Tokens(
      val input : Context,
      val index : Int,
      multipleStatements : Boolean,
      stack : List[Boolean],
      last : ScalaToken)
      extends Input[ScalaToken, Tokens] {
    
    lazy val next = tryNext(input)

    private def tryNext(context : Context) : rules.Result[ScalaToken, Tokens] = token(context) match {
      case Success(token, context) => token match {
        case Newline if !isNewlineAllowed => tryNext(context)
        case OpenCurly => next(token, context, true :: multipleStatements :: stack)
        case OpenRound | OpenSquare => next(token, context, false :: multipleStatements :: stack)
        case CloseRound | CloseSquare | CloseCurly => next(token, context, stack)
        case _ => next(token, context, multipleStatements :: stack)
      }
      case _ => Failure[Tokens]
    }
    
    private def isNewlineAllowed = multipleStatements && (last match {
      case OpenCurly | OpenRound | OpenSquare => false
      case reservedId : ReservedId => reservedId.canTerminateStatement
      case _ => true
    })
    
    private def next(token : ScalaToken, context : Context, stack : List[Boolean]) = {
      val head :: tail = stack match { case Nil => true :: Nil case _ => stack }
      Success(token, new Tokens(context, index + 1, head, tail, token))
    }

    override def toString = input toString
  }
  
  def tokens(input : Context) = new Tokens(input, 0, true, Nil, null)

  val space = choice(" \t") -^ Space
   

  val decimalDigit = ('0' to '9') ^^ (_ - 48L)
  def decimal(n : Long) = decimalDigit ^^ (n * 10 + _)
  def decimalN(n : Long) : Rule[Long] = decimal(n) >> decimalN | success(n)
  
  val octalDigit = decimalDigit.filter(_ < 8)
  def octal(n : Long) = octalDigit ^^ (n * 8 + _)
  def octalN(n : Long) : Rule[Long] = octal(n) >> octalN | success(n)
  
  val hexDigit = decimalDigit | ('A' to 'F') ^^ (_ - 55L) | ('a' to 'f') ^^ (_ - 87L)
  def hex(n : Long) = hexDigit ^^ (n * 16 + _)
  def hexN(n : Long) : Rule[Long] = hex(n) >> hexN | success(n)

  val unicodeEscape = "\\u" -~ hexDigit >> hex >> hex >> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> octal >> octal ^^ { _.asInstanceOf[Char] }
 
  val charEscapeSeq = '\\' -~ ( choice("\"\'\\")
      | 'b' -^ '\b' | 't' -^ '\t' | 'n' -^ '\n' | 'f' -^ '\f' | 'r' -^ '\r') 

  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val letter = choice("$_") | (anyChar filter isLetter)
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  lazy val idRest : Rule[List[Char]] = ('_' ~++ (opChar+)) ~- !idChar | !idChar -^ Nil | idChar ~++ idRest
  
  val quoteId = '`' -~ (printableChar +~- '`') ^^ toString
  val unquotedId = letter ~++ idRest ^^ toString
  val op = (opChar+) ^^ toString
  
  val keyword = unquotedId.filter(ReservedId(_)) ^^ Keyword
  val reservedOp = op.filter(ReservedId(_)) ^^ ReservedOperator
  val reservedId : Rule[ReservedId] = keyword | reservedOp
  
  val id = (quoteId ^^ QuotedId
      | op.filter(!ReservedId(_)) ^^ Operator
      | ((lower&)-?) ~ unquotedId.filter(!ReservedId(_)) ^~^ UnquotedId)
  
  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' -^ 0L
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~ (choice("Ll")-?) ~- !idChar ^~^ integerOrLongLiteral
  
  private def integerOrLongLiteral(value : Long, isLong : Boolean) = 
    if (isLong) LongLiteral(value)
    else IntegerLiteral(value.asInstanceOf[Int])
  
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (('0' to '9')*) ^^ toString) | ""
  val exponentPart = (for (e <- choice("eE"); s <- "+" | "-" | ""; n <- intPart) yield e + s + n) | ""

  val floatLiteral = for {
    i <- intPart; f <- floatPart 
    val m = i + f if m != "" && m != "."
    e <- exponentPart; q <- choice("fF")
  } yield FloatLiteral((m + e) toFloat)
  
  val doubleLiteral = for {
    i <- intPart; f <- floatPart 
    val m = i + f if m != "" && m != "."
    e <- exponentPart; q <- (choice("dD") -?)
    if f != "" || e != "" || q
  } yield DoubleLiteral((m + e) toDouble)
  
  val charElement = charEscapeSeq | printableChar
  val characterLiteral = '\'' -~ (charElement - '\'') ~- '\'' ^^ CharacterLiteral
  val stringLiteral = ('\"' -~ charElement *~- '\"' | "\"\"\"" -~ anyChar *~- "\"\"\"") ^^ toString ^^ StringLiteral
  val symbolLiteral = '\'' -~ (unquotedId | op) ^^ Symbol ^^ SymbolLiteral
  
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = (singleLineComment | multiLineComment) -^ Comment
  
  val nl = (space | comment *) -~ newline -^ Newline
  val delimiter = choice(";.,()[]{}")
  
  val literal : Rule[Literal] = ("null" -~ !idChar -^ Null
      | "true" -~ !idChar -^ True
      | "false" -~ !idChar -^ False
      | integerLiteral 
      | characterLiteral 
      | stringLiteral 
      | symbolLiteral
      | floatLiteral 
      | doubleLiteral)
    
  val statementStart : Rule[ScalaToken] = (space | comment *) -~ (
      '(' -^ OpenRound
      | '{' -^ OpenCurly
      | literal
      | reservedId.filter(_.canStartStatement)
      | id)
      
  val statementPart : Rule[ScalaToken] = (newline | space | comment *) -~ (
      ';' -^ Semicolon
      | '.' -^ Dot
      | ',' -^ Comma
      | ')' -^ CloseRound
      | '[' -^ OpenSquare
      | ']' -^ CloseSquare
      | '}' -^ CloseCurly
      | reservedId.filter(!_.canStartStatement))
  
  val token : Rule[ScalaToken] = statementPart | nl | statementStart
      

}
