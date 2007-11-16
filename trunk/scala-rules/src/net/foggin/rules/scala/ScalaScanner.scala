package net.foggin.rules.scala

import Character._

/** Defines the Scala lexical syntax rules.
 *
 * Note: Embedded XML not yet implemented.
 */
abstract class ScalaScanner extends Scanner {
  
  def token[T](rule : Rule[T]) = (space | comment *) -~ rule

  val space = choice(" \t")
  val nl = token(newline) -^ "{nl}"
  val semi = (token(";") | nl) ~- (space | newline *)
   
  val reserved = _root_.scala.collection.immutable.Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@")
  
  def isReserved(id : String) = reserved.contains(id)
      
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
      | 'b' -^ '\b' | 't' -^ '\t' | 'n' -^ '\n' | 'f' -^ '\f' | 'r' -^ '\r') 

  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val letter = choice("$_") | (anyChar filter isLetter)
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  
  val quoteId = '`' -~ (printableChar +~- '`') ^^ toString
  val unquotedId = ((opChar+) | letter ~++ idRest) ^^ toString
  val plainId = unquotedId filter { id => !reserved.contains(id) }
  
  val keyword = token(letter ~++ idRest) ^^ toString filter isReserved
  val reservedOp = token(opChar+) ^^ toString filter isReserved
  
  val varId = token((lower&) -~ plainId)
  val id = token(quoteId | plainId)
  def name(id : String) = token(unquotedId filter (_ == id))

  lazy val idRest : Rule[List[Char]] = ('_' ~++ (opChar+)) ~- !idChar | !idChar -^ Nil | idChar ~++ idRest

  val `abstract` = name("abstract")
  val `case` = name("case")
  val `catch` = name("catch")
  val `class` = name("class")
  val `def` = name("def")
  val `do` = name("do")
  val `else` = name("else")
  val `extends` = name("extends")
  val `false` = name("false") -^ Literal(false)
  val `final` = name("final")
  val `finally` = name("finally")
  val `for` = name("for")
  val `forSome` = name("forSome")
  val `if` = name("if")
  val `implicit` = name("implicit")
  val `import` = name("import")
  val `lazy` = name("lazy")
  val `match` = name("match")
  val `new` = name("new")
  val `null` = name("null") -^ Literal(null)
  val `object` = name("object")
  val `override` = name("override")
  val `package` = name("package")
  val `private` = name("private")
  val `protected` = name("protected")
  val `requires` = name("requires")
  val `return` = name("return")
  val `sealed` = name("sealed")
  val `super` = name("super")
  val `this` = name("this")
  val `throw` = name("throw")
  val `trait` = name("trait")
  val `try` = name("try")
  val `true` = name("true") -^ Literal(true)
  val `type` = name("type")
  val `val` = name("val")
  val `var` = name("var")
  val `while` = name("while")
  val `with` = name("with")
  val `yield` = name("yield")
  val `_` = name("_")
  
  val `:` = name(":")
  val `=` = name("=")
  val `=>` = name("=>") | name("\u21D2")
  val `<-` = name("<-")
  val `<:` = name("<:")
  val `<%` = name("<%")
  val `>:` = name(">:")
  val `#` = name("#")
  val `@` = name("@")
  
  // not reserved, but have special uses:
  val minus = name("-")  // variance or unary op
  val plus = name("+") // variance or unary op
  val bang = name("!") // unary op
  val tilde = name("~") // unary op
  val `*` = name("*") // repeated params
  val `|` = name("|") // combining patterns
  
  // separators
  val comma = token(',')
  val dot = token('.')
  val openRound= token('(')
  val closeRound = token(')')
  val openSquare = token('[')
  val closeSquare = token(']')
  val openCurly = token('{')
  val closeCurly = token('}')
  
  val separator = comma | dot | openRound | closeRound | openSquare | closeSquare | openCurly | closeCurly
  
  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' -^ 0
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
  
  val booleanLiteral = `true` | `false`

  val charElement = charEscapeSeq | printableChar
  val characterLiteral = '\'' -~ (charElement - '\'') ~- '\''
  val stringLiteral = ('\"' -~ charElement *~- '\"' | "\"\"\"" -~ anyChar *~- "\"\"\"") ^^ toString
  val symbolLiteral = '\'' -~ plainId ^^ Symbol
  
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
  val literal = (booleanLiteral | `null`
      | token(integerLiteral | floatLiteral | doubleLiteral | characterLiteral | stringLiteral | symbolLiteral) ^^ (Literal(_)))
  

}


