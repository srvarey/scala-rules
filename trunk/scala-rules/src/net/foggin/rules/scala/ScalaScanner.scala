package net.foggin.rules.scala

import Character._

object ScalaToken {
  
  /** Reserved ids that can terminate a statement */
  val endStatements = _root_.scala.collection.immutable.Set(
      "this", "null", "true", "false", "return", "type", "_")
    
  /** Reserved ids that cannot start a statement */
  val cannotStartStatements = _root_.scala.collection.immutable.Set(
      "catch", "else", "extends", "finally", "forSome", "match", "requires", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "\u21D2")

  def canStartStatement(token : ScalaToken) = token match {
    case OpenRound | OpenCurly => true
    case _ : Delimiter => false
    case reserved : ReservedId => !cannotStartStatements.contains(reserved.id)
    case _ => true
  }

  def canEndStatement(token : ScalaToken) = token match {
    case null => false
    case CloseRound | CloseSquare | CloseCurly => true
    case _ : Delimiter => false
    case reserved : ReservedId => endStatements.contains(reserved.id)
    case _ => true
  }
}

import ScalaToken._

abstract class ScalaToken {
  
}

abstract class ReservedId extends ScalaToken {
  def id : String
}

object ReservedId {
  val reserved = _root_.scala.collection.immutable.Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")

  def apply(id : String) = isReserved(id)
  def isReserved(id : String) = reserved.contains(id)
}

case class Keyword(id : String) extends ReservedId
case class ReservedOperator(id : String) extends ReservedId

abstract class Delimiter extends ScalaToken
case object Newline extends Delimiter
case object Semicolon extends Delimiter
case object Dot extends Delimiter
case object Comma extends Delimiter
case object OpenRound extends Delimiter
case object CloseRound extends Delimiter
case object OpenSquare extends Delimiter
case object CloseSquare extends Delimiter
case object OpenCurly extends Delimiter
case object CloseCurly extends Delimiter

abstract class IdToken extends ScalaToken { def id : String }

case class Operator(id : String) extends IdToken
case class QuotedId(id : String) extends IdToken
case class VariableId(id : String) extends IdToken
case class NonVariableId(id : String) extends IdToken

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

abstract class XMLToken extends ScalaToken
case object XMLComment extends XMLToken
case class StartElement(name : String) extends XMLToken
case class EndElement(name : String) extends XMLToken
case class AttributeName(name : String) extends XMLToken
case class AttributeValue(value : String) extends XMLToken
case object EmptyElement extends XMLToken
case object ElementContent extends XMLToken
case class TextContent(text : String) extends XMLToken



/** Defines the Scala lexical syntax rules.
  *
  * Note: Embedded XML not yet implemented.
  */
abstract class ScalaScanner extends Scanner {
  
  /**
   * Input for Scala Parser.
   */
  class Tokens(val input : Context, val index : Int,
      multipleStatements : List[Boolean],
      xml : boolean,
      last : ScalaToken)
      extends Input[ScalaToken, Tokens] {
    
    lazy val next = tokenAhead(input) match {
      case Success(true ~ token, _) if isNewlineAllowedBefore(token) => nextNewline
      case Success(_ ~ token, context) => token match {
          case OpenCurly => nextToken(token, context, true :: multipleStatements)
          case OpenRound | OpenSquare => nextToken(token, context, false :: multipleStatements)
          case CloseRound | CloseSquare | CloseCurly => nextToken(token, context, multipleStatements.tail)
          case _ => nextToken(token, context, multipleStatements)
        }
      case _ => Failure[Tokens]
    }
  
    
    private def isNewlineAllowedBefore(token : ScalaToken) = (!xml 
        && multipleStatements.head 
        &&  canEndStatement(last) 
        && canStartStatement(token))
        
    private def nextNewline = nl(input) match {
      case Success(token, context) => nextToken(token, context, multipleStatements)
      case _ => Failure[Tokens]
    }
    
    private def nextToken(token : ScalaToken, context : Context, multipleStatements : List[Boolean]) = 
      Success(token, new Tokens(context, index + 1, if (multipleStatements == Nil) true :: Nil else multipleStatements, xml, token))

    override def toString = input toString
  }
  
  def tokens(input : Context) = new Tokens(input, 0, true :: Nil, false, null)

   

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
  
  val quoteId = '`' -~ (printableChar +~- '`') ^^ toString ^^ QuotedId
  val varId = notReserved(lower ~++ idRest) ^^ VariableId
  val nonVarId = notReserved(letter ~++ idRest) ^^ NonVariableId
  val op = notReserved(opChar+) ^^ Operator
  val plainId = op | varId | nonVarId
  val id = quoteId | plainId
  
  val keyword = reserved(letter ~++ idRest) ^^ Keyword
  val reservedOp = reserved(opChar+) ^^ ReservedOperator
  val reservedId = keyword | reservedOp
  
  def reserved(rule : Rule[Seq[Char]]) = rule ^^ toString filter { id => ReservedId(id) }
  def notReserved(rule : Rule[Seq[Char]]) = rule ^^ toString filter { id => !ReservedId(id) }
  
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
  val symbolLiteral = '\'' -~ plainId ^^ { token => SymbolLiteral(Symbol(token.id)) }
  
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = (singleLineComment | multiLineComment) -^ Comment
  
  val space = choice(" \t") -^ Space
  val nl = (space | comment *) -~ newline -^ Newline
  
  val delimiter = (';' -^ Semicolon
    | '.' -^ Dot
    | ',' -^ Comma
    | '(' -^ OpenRound
    | ')' -^ CloseRound
    | '[' -^ OpenSquare
    | ']' -^ CloseSquare
    | '{' -^ OpenCurly
    | '}' -^ CloseCurly)
  
  val literal : Rule[Literal] = ("null" -~ !idChar -^ Null
      | "true" -~ !idChar -^ True
      | "false" -~ !idChar -^ False
      | integerLiteral 
      | characterLiteral 
      | stringLiteral 
      | symbolLiteral
      | floatLiteral 
      | doubleLiteral)
    
      /*
  val statementStart : Rule[ScalaToken] = (space | comment *) -~ (
      '(' -^ OpenRound
      | '{' -^ OpenCurly
      | literal
      | reservedId.filter(_.canStartStatement)
      | id)
      
  val statementPart : Rule[ScalaToken] = (nl | space | comment *) -~ (
      ';' -^ Semicolon
      | '.' -^ Dot
      | ',' -^ Comma
      | ')' -^ CloseRound
      | '[' -^ OpenSquare
      | ']' -^ CloseSquare
      | '}' -^ CloseCurly
      | reservedId.filter(!_.canStartStatement))
      
      */
        
  val tokenAhead = (nl-?) ~- (nl*) ~ token
  
  val token : Rule[ScalaToken] = (space | comment *) -~ (delimiter | literal | reservedId | id)
      
  val xmlNameStart = (elem('_')
      | unicode(LOWERCASE_LETTER) // Ll
      | unicode(UPPERCASE_LETTER) // Lu
      | unicode(OTHER_LETTER ) // Lo
      | unicode(TITLECASE_LETTER) //Lt
      | unicode(LETTER_NUMBER)) // Nl
      
  val xmlNameChar = (xmlNameStart | choice(".-")
      | unicode(COMBINING_SPACING_MARK) // Mc
      | unicode(ENCLOSING_MARK) // Me
      | unicode(NON_SPACING_MARK) // Mn
      | unicode(MODIFIER_LETTER) // Lm
      | unicode(DECIMAL_DIGIT_NUMBER )) // Nd
      
  val xmlName = xmlNameStart ~++ (xmlNameChar*) ^^ toString
      
  val xmlS = choice(" \t\r\n")+
    
  val xmlComment = "<!--" -~ anyChar *~- "-->"
    
  val char1 = anyChar - choice("<&")
  
  val reference = ("&amp;" -^ '&'
      | "&lt;" -^ '<'
      | "&gt;" -^ '>'
      | "&apos;" -^ '\''
      | "&quot;" -^ '"')
    
  val attributeName = xmlS -~ xmlName ~- '=' ^^ AttributeName
  val attributeValue = ('"' -~ (char1 | reference) *~- '"' 
      | '\'' -~ (char1 | reference) *~- '\'') ^^ toString ^^ AttributeValue
      // | scalaExpr
      
  
  val startElement = '<' -~ xmlName ^^ StartElement
  val endElement = "</" -~ xmlName ~- (xmlS?) ~- '>' ^^ EndElement
  val emptyElement = (xmlS?) ~- "/>" -^ EmptyElement
  val elementContent = (xmlS?) ~- '>' -^ ElementContent
    
  val charData = ("{{" -^ '{' | char1 - ("]]>" | '{') +)
  
  val attribute = attributeName ~ attributeValue
  lazy val element : Rule[Any] = startElement ~ (attribute*) ~ (emptyElement | elementContent ~ xmlContent ~- endElement)
  lazy val xmlContent = element | comment | charData | reference // | scalExpr  | cdataSect | pi 
    
  /*

XmlExpr ::= XmlContent {Element}

Element ::= EmptyElemTag
| STag Content ETag

EmptyElemTag ::= ‘<’ Name {S Attribute} [S] ‘/>’
STag ::= ‘<’ Name {S Attribute} [S] ‘>’
ETag ::= ‘</’ Name [S] ’>’
Content ::= [CharData] {Content1 [CharData]}
Content1 ::= XmlContent
| Reference
| ScalaExpr
XmlContent ::= Element
| CDSect
| PI
| Comment
Attribute ::= Name Eq AttValue
AttValue ::= ‘"’ {CharQ | CharRef} ‘"’
| ‘’’ {CharA | CharRef} ‘’’
| ScalaExp
ScalaExpr ::= ‘{’ expr ‘}’
CharData ::= { CharNoRef } without {CharNoRef}‘{’CharB {CharNoRef}
and without {CharNoRef}‘]]>’{CharNoRef}

BaseChar, Char, Comment, CombiningChar, Ideographic, NameChar, S, Reference
::= “as in W3C XML”

Char1 ::= Char without ‘<’ | ‘&’
CharQ ::= Char1 without ‘"’
CharA ::= Char1 without ‘’’
CharB ::= Char1 without ’{’
Name ::= XNameStart {NameChar}
XNameStart ::= ‘_’ | BaseChar | Ideographic
(as in W3C XML, but without ‘:’

XmlPattern ::= ElementPattern
ElemPattern ::= EmptyElemTagP
| STagP ContentP ETagP
EmptyElemTagP ::= ’<’ Name [S] ’/>’
STagP ::= ’<’ Name [S] ’>’
ETagP ::= ’</’ Name [S] ’>’
ContentP ::= [CharData] {(ElemPattern|ScalaPatterns) [CharData]}
ContentP1 ::= ElemPattern
| Reference
| CDSect
| PI
| Comment
| ScalaPatterns
ScalaPatterns ::= ’{’ patterns ’}’



  */
  
}
