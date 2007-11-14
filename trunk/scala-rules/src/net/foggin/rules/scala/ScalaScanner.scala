package net.foggin.rules.scala

import Character._

/** Defines the Scala lexical syntax rules.
 *
 * Note: Embedded XML not yet implemented.
 */
abstract class ScalaScanner extends Scanner {
  
  def token[T](rule : Rule[T]) = (choice(" \t") | comment *) -~ rule

  // reserved keywords and operators
  val keywords = _root_.scala.collection.mutable.Map.empty[String, Rule[String]]
  val reservedOps = _root_.scala.collection.mutable.Map.empty[String, Rule[String]]
      
  private def keyword(name : String) = keywords.getOrElseUpdate(name, token(name ~- !idChar))
  private def reservedOp(name : String) = reservedOps.getOrElseUpdate(name, opToken(name))
  private def opToken(op : String) = token(op ~- !opChar)
  
  val `abstract` = keyword("abstract")
  val `case` = keyword("case")
  val `catch` = keyword("catch")
  val `class` = keyword("class")
  val `def` = keyword("def")
  val `do` = keyword("do")
  val `else` = keyword("else")
  val `extends` = keyword("extends")
  val `false` = keyword("false")
  val `final` = keyword("final")
  val `finally` = keyword("finally")
  val `for` = keyword("for")
  val `forSome` = keyword("forSome")
  val `if` = keyword("if")
  val `implicit` = keyword("implicit")
  val `import` = keyword("import")
  val `lazy` = keyword("lazy") // NB "lazy" is missing from the list in language specification 2.6.0
  val `match` = keyword("match")
  val `new` = keyword("new")
  val `null` = keyword("null")
  val `object` = keyword("object")
  val `override` = keyword("override")
  val `package` = keyword("package")
  val `private` = keyword("private")
  val `protected` = keyword("protected")
  val `requires` = keyword("requires")
  val `return` = keyword("return")
  val `sealed` = keyword("sealed")
  val `super` = keyword("super")
  val `this` = keyword("this")
  val `throw` = keyword("throw")
  val `trait` = keyword("trait")
  val `try` = keyword("try")
  val `true` = keyword("true")
  val `type` = keyword("type")
  val `val` = keyword("val")
  val `var` = keyword("var")
  val `while` = keyword("while")
  val `with` = keyword("with")
  val `yield` = keyword("yield")
  val `_` = keyword("_")
  
  val `:` = reservedOp(":")
  val `=` = reservedOp("=")
  val `=>` = reservedOp("=>") | reservedOp("\u21D2")
  val `<-` = reservedOp("<-")
  val `<:` = reservedOp("<:")
  val `<%` = reservedOp("<%")
  val `>:` = reservedOp(">:")
  val `#` = reservedOp("#")
  val `@` = reservedOp("@")
  
  // not reserved, but have special uses:
  val minus = opToken("-")  // variance or unary op
  val plus = opToken("+") // variance or unary op
  val bang = opToken("!") // unary op
  val tilde = opToken("~") // unary op
  val `*` = opToken("*") // repeated params
  val `|` = opToken("|") // combining patterns
  
  //These can be defined but cause compile errors when used later
  //val `+` = opToken("+")
  //val `-` = opToken("-")
  //val `!` = opToken("!") // unary op
  //val `~` = opToken("~")

  
  //These can be defined but cause runtime errors when used in plugin
  //val `,` = token(',') 
  //val `.` = token('.')
  //val `(` = token('(')
  //val `)` = token(')')
  //val `[` = token('[')
  //val `]` = token(']')
  //val `{` = token('{')
  //val `}` = token('}')
  
  val comma = token(',')
  val dot = token('.')
  val separator = token(choice(",.()[]{}"))
  
  val decimalDigit = range('0', '9') ^^ (_ - 48L)
  def decimal(n : Long) = decimalDigit ^^ (n * 10 + _)
  def decimalN(n : Long) : Rule[Long] = decimal(n) >> decimalN | success(n)
  
  val octalDigit = decimalDigit.filter(_ < 8)
  def octal(n : Long) = octalDigit ^^ (n * 8 + _)
  def octalN(n : Long) : Rule[Long] = octal(n) >> octalN | success(n)
  
  val hexDigit = decimalDigit | range('A', 'F') ^^ (_ - 55L) | range('a', 'f') ^^ (_ - 87L)
  def hex(n : Long) = hexDigit ^^ (n * 16 + _)
  def hexN(n : Long) : Rule[Long] = hex(n) >> hexN | success(n)

  val unicodeEscape = "\\u" -~ hexDigit >> hex >> hex >> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> octal >> octal ^^ { _.asInstanceOf[Char] }
 
  val charEscapeSeq = '\\' -~ ( choice("\"\'\\")
      | 'b' ^^^ '\b' | 't' ^^^ '\t' | 'n' ^^^ '\n'
      | 'f' ^^^ '\f' | 'r' ^^^ '\r') 

  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  //val delimiter = choice("`'\".;,")
  //val separator = parentheses | delimiter | whitespace
  
  val letter = choice("$_") | (anyChar filter isLetter)
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  
  val keyword = select(keywords.values.toList)
  val reservedOp = select(reservedOps.values.toList)
  
  val op = token((opChar+) ^^ toString filter (!reservedOps.contains(_)))
  val varid = token(lower ~++ idRest ^^ toString filter (!keywords.contains(_)))
  val plainid = op | token(letter ~++ idRest ^^ toString filter (!keywords.contains(_)))
  val quoteid = token('`' -~ (printableChar +~- '`')) ^^ toString
  val id = plainid | quoteid

  lazy val idRest : Rule[List[Char]] = ('_' ~++ (opChar+)) ~- !idChar | !idChar ^^^ Nil | idChar ~++ idRest

  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' ^^^ 0
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?) ~- !idChar
  
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (range('0', '9')*) ^^ toString) | ""
  val exponentPart = (for (e <- choice("eE"); s <- "+" | "-" | ""; n <- intPart) yield e + s + n) | ""

  val floatLiteral = for {
    i <- intPart; f <- floatPart 
    val m = i + f if m != "" && m != "."
    e <- exponentPart; q <- choice("fF")
  } yield m + e
  
  val doubleLiteral = for {
    i <- intPart; f <- floatPart 
    val m = i + f if m != "" && m != "."
    e <- exponentPart; q <- (choice("dD") ?)
    if f != "" || e != "" || q != None
  } yield m + e
  
  val booleanLiteral = "true" | "false"

  val charElement = charEscapeSeq | printableChar
  val characterLiteral = '\'' -~ (charElement - '\'') ~- '\''
  val stringLiteral = ('\"' -~ charElement *~- '\"' | "\"\"\"" -~ anyChar *~- "\"\"\"") ^^ toString
  val symbolLiteral = '\'' -~ plainid ^^ Symbol
  
  val space = (choice(" \t")*) ^^^ " "
  val nl = (space ~ newline ~ space) ^^^ "{nl}"
  val semi = space ~ (";" | nl) ~ (nl*) ~ space ^^^ ";"
   
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = singleLineComment | multiLineComment
  
  /** Literal ::= integerLiteral
   *    | floatingPointLiteral
   *    | booleanLiteral
   *    | characterLiteral
   *    | stringLiteral
   *    | symbolLiteral
   *    | null */
  val literal = token(integerLiteral | floatLiteral | doubleLiteral | booleanLiteral | characterLiteral | stringLiteral | symbolLiteral | `null`) ^^ (Literal(_))
  

}


