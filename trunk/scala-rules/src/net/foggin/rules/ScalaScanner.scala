package net.foggin.rules

import Character._

abstract class ScalaScanner extends Scanner {

  // NB "lazy" is missing from my version of the language specification
  val reserved = Set("abstract", "case", "catch", "class", "def",
      "do", "else", "extends", "false", "final", 
      "finally", "for", "forSome", "if", "implicit", 
      "import", "lazy", "match", "new", "null", "object", 
      "override", "package", "private", "protected", "requires", 
      "return", "sealed", "super", "this", "throw", 
      "trait", "try", "true", "type", "val", "var", "while", "with", "yield", 
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")
  
  
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
 
  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val parentheses = choice("()[]{}")
  val delimiter = choice("`'\".;,")
  val separator = parentheses | delimiter | whitespace
  
  val letter = anyChar filter isLetter
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idStart = choice("$_") | letter
  val idChar = idStart | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  
  val op = opChar+
  val varid = lower ~++ idRest
  val plainid = (op | varid | idStart ~++ idRest | varid | op) ^^ literal filter (!reserved.contains(_))
  val quoteid = delimit_+(printableChar, '`') ^^ literal
  val id = plainid | quoteid

  lazy val idRest : Rule[List[Char]] = !idChar ^^^ Nil | ('_' ~++ op) ~- !idChar | idChar ~++ idRest

  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' ^^^ 0
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?) ~- !idChar
  
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (range('0', '9')*) ^^ literal) | ""
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

  val charEscapeSeq = '\\' ~- ( choice("\"\'\\")
      | 'b' ^^^ '\b' | 't' ^^^ '\t' | 'n' ^^^ '\n'
      | 'f' ^^^ '\f' | 'r' ^^^ '\r') 

  val charElement = charEscapeSeq | printableChar
  val characterLiteral = delimit(charElement, '\'')
  val stringLiteral = (delimit_*(charElement, '\"') | delimit_*(anyChar, "\"\"\"")) ^^ literal
  val symbolLiteral = '\'' ~ plainid
  
  
  val semi = (";" | newline) ~- (newline*)
   
  val other = (item filter { ch => !(ch isWhitespace) } +) ^^ literal

  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = bracket_*("/*", multiLineComment | anyChar, "*/") ^^ literal
  val lineComment : Rule[String] = bracket_*("//", item, newline.unary_&) ^^ literal
  val comment = lineComment | multiLineComment
}

abstract class IncrementalScalaScanner extends ScalaScanner with IncrementalScanner {
  val token = memo("token", (whitespace?) -~ (parentheses | stringLiteral | comment | id | delimiter| other))
  val tokens = view(token) _

  val line = memo("line", newline ^^^ "" | (!newline -~ item +) ~- (newline?) ^^ literal)
  val lines = view(line) _
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
  checkFailure(id)(reserved.toList : _*)
  
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

object TestIncrementalScalaScanner extends IncrementalScalaScanner with Application {
  
  //println(octalEscape("\\060"))


  // do some incremental parsing
  var input = new EditableInput[Char]

  def printTokens() {
    println; println("Tokens: ")
    println(tokens(input).mkString("\"", "\", \"", "\""))
  }

  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  input = input.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  printTokens()
  printLines()

 // insert something
 input = input.edit(19, 0, """
   class Dummy {
     val answer = 42
   }
   """)

  printTokens()
  printLines()
} 