package net.foggin.rules

import Character._

/** Defines the Scala lexical syntax rules.
 *
 * Note: Embedded XML not yet implemented.
 */
abstract class ScalaScanner extends Scanner {
  
  def ignoreable = choice(" \t\r\n\f") | comment
  
  def token[T](rule : Rule[T]) = (ignoreable*) -~ rule

  // reserved keywords and operators
  val keywords = scala.collection.mutable.Map.empty[String, Rule[String]]
  val reservedOps = scala.collection.mutable.Map.empty[String, Rule[String]]
      
  private def keyword(name : String) = keywords.getOrElseUpdate(name, token(name ~- !idChar))
  private def reservedOp(name : String) = reservedOps.getOrElseUpdate(name, token(name ~- !opChar))
  
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
  
  //These fail when used in Eclipse plugin
  //val `,` = token(',') 
  //val `.` = token('.')
  //val `(` = token('(')
  //val `)` = token(')')
  //val `[` = token('[')
  //val `]` = token(']')
  //val `{` = token('{')
  //val `}` = token('}')
  
  val comma = token(',')
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
 
  val charEscapeSeq = '\\' ~- ( choice("\"\'\\")
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
  val symbolLiteral = '\'' ~ plainid
  
  val space = (choice(" \t")*) ^^^ " "
  val nl = (space ~ newline ~ space) ^^^ "{nl}"
  val semi = space ~ (";" | nl) ~ (nl*) ~ space ^^^ ";"
   
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  val comment = singleLineComment | multiLineComment
  
  /** Literal ::= integerLiteral
   *    | floatingPointLiteral
   *    | booleanLiteral
   *    | characterLiteral
   *    | stringLiteral
   *    | symbolLiteral
   *    | null */
  val literal = token(integerLiteral | floatLiteral | doubleLiteral | booleanLiteral | characterLiteral | stringLiteral | symbolLiteral | `null`) ^^ (Literal(_))
  

}




object TestScalaScanner extends ScalaScanner with Application {
  type Context = ArrayInput[Char]
  
  def check[A](input : String, actual : Result[A], expected : Result[A]) {
    (expected, actual) match {
      case (Success(ea, es), Success(aa, as)) if ea == aa && es.toString == as.toString => ()
      case (e, a) if e == a => ()
      case _ => error ("Input: " + input + 
        "\nExpected result: " + expected + 
        "\nActual result: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[A])(input : String *) {
    for (i <- input) check(i, rule(i), Failure[Context])
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, Result[A]) *) {
    for ((input, result) <- expect) check(input, rule(input), result)
  }
  
  implicit def anyToSuccess[A](a : A) : Result[A] = Success(a, "")
  
  implicit def tripleToSuccess[A](triple : ((String, A), String)) : (String, Result[A]) = 
    triple match { case ((input, a), rest) => input -> Success(a, rest) }
  
  checkRule(unicodeEscape)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(octalEscape)("\\061" -> '1')
  checkRule(anyChar)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(opChar)("\\u21D2" -> '\u21D2')
  
  checkFailure(integerLiteral)("l", "L", "0x")
  
  checkRule(integerLiteral) (
      "0l" -> 0,
      "12 " -> 12 -> " ",
      "012" -> 10,
      "0x12" -> 18)
      
  checkFailure(opChar)(".", ";", "(", "[", "}")
  
  checkRule(opChar) (
      "+" -> '+',
      "-" -> '-',
      "*" -> '*',
      "/" -> '/')
   
      
  // check reserved words aren't ids
  checkFailure(id)(keywords.keys.toList : _*)
  checkFailure(id)(reservedOps.keys.toList : _*)
  
  //checkRule(keyword)(keywords.keys.toList.map[String] { s => (s, s) } : _*)
  
  checkRule(keyword)(
      "abstract" -> "abstract",
      "_" -> "_")
  
  checkRule(quoteid)("`yield`" -> "yield")
  
  checkRule(id)(
      "`yield`" -> "yield", 
      "yield1" -> "yield1", 
      "yield_+" -> "yield_+",
      "`\\u21D2`" -> "\u21D2")

  checkRule(floatLiteral)(
      "1f" -> "1", 
      "1.0F" -> "1.0", 
      "1.e2F" -> "1.e2",
      ".12E3f" -> ".12E3")

  checkRule(doubleLiteral)(
      "1D" -> "1", 
      "1.0" -> "1.0", 
      "1e2" -> "1e2",
      ".12E3D" -> ".12E3")

  println("Scanner tests passed")
}

object TestIncrementalScalaScanner extends ScalaScanner with IncrementalScanner with Application {
  //val token = memo("token", space -~ (nl | semi | parentheses | integerLiteral | characterLiteral | symbolLiteral | stringLiteral | comment| keyword | reservedOp | id  | delimiter))
  val tokens = view(memo("token", nl | semi | token(comment) | separator |  literal | keyword | reservedOp | id)) _

  val line = memo("line", newline ^^^ "" | (!newline -~ item +) ~- (newline?) ^^ toString)
  val lines = view(line) _

  var input = new EditableInput[Char]

  def printTokens() {
    println; println("Tokens: ")
    println(tokens(input).mkString(", "))
  }

  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  input.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  printTokens()
  printLines()

 // insert something
 input.edit(19, 0, """
   class Dummy {
     val answer = 42
   }
   """)

  printTokens()
  printLines()
} 