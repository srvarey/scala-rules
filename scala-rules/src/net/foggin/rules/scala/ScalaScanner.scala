package net.foggin.rules.scala

import Character._
import ScalaToken._

  sealed abstract class State
  case object MultipleStatements extends State
  case object SingleStatement extends State
  case object XML extends State

  case class TokenState(token : ScalaToken, states : List[State]) {
    
    def state = states match {
      case head :: tail => head
      case _ => MultipleStatements
    }
    
    def previousStates = states match {
      case head :: tail => tail
      case _ => Nil
    }
    
    def isNewlineAllowedBefore(next : ScalaToken) =
        state == MultipleStatements && canEndStatement(token) && canStartStatement(next)

    def next(token : ScalaToken) = token match {
      case OpenCurly => TokenState(token, MultipleStatements :: states)
      case OpenRound | OpenSquare => TokenState(token, SingleStatement :: states)
      case CloseRound | CloseSquare | CloseCurly => TokenState(token, previousStates)
      case _ => TokenState(token, states)
    }
  }
  

/** Defines the Scala lexical syntax rules.
  *
  * Note: Embedded XML under construction!
  *
  * @author Andrew Foggin, based on Scala Language Specification
  */
abstract class ScalaScanner extends Scanner {
  
  /**
   * Input for Scala Parser.
   */
  class Tokens(val input : Context, val index : Int, state : TokenState) extends Input[ScalaToken, Tokens] {
    
     lazy val next = nextToken(state)(input) match {
       case Success(state, context) => Success(state.token, new Tokens(context, index + 1, state))
       case _ => Failure[Tokens]
     }

    override def toString = input toString
  }
  
  def tokens(input : Context) = new Tokens(input, 0, TokenState(null, Nil))

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
    
  val xmlComment = "<!--" -~ anyChar *~- "-->" -^ XMLComment
    
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
  lazy val xmlContent = element | xmlComment | charData | reference // | scalExpr  | cdataSect | pi 
    
  def startXML(lastToken : ScalaToken) = lastToken match {
    case OpenRound | OpenCurly | Newline => startElement
    case _ => (space*) -~ startElement
  }
  
  val token : Rule[ScalaToken] = (newline | space | comment *) -~ (delimiter | literal | reservedId | id)
  
  def nlToken(state : TokenState) = nl ~- (token.filter(state.isNewlineAllowedBefore) &)
      
  def nextToken(state : TokenState) = (nlToken(state) | token) ^^ state.next

  
  
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
