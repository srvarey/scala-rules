package net.foggin.rules

import Character._

abstract class ScalaScanner extends Scanner {

  val decimalDigit = range('0', '9') ^^ (_ - 48)
  def decimal(n : Int) = decimalDigit ^^ (n * 10 + _)
  def decimalN(n : Int) : Rule[Int] = decimal(n) >> decimalN | success(n)
  
  val octalDigit = decimalDigit.filter(_ < 8)
  def octal(n : Int) = octalDigit ^^ (n * 8 + _)
  def octalN(n : Int) : Rule[Int] = octal(n) >> octalN | success(n)
  
  val hexDigit = decimalDigit | range('A', 'F') ^^ (_ - 55) | range('a', 'f') ^^ (_ - 87)
  def hex(n : Int) = hexDigit ^^ (n * 16 + _)
  def hexN(n : Int) : Rule[Int] = hex(n) >> hexN | success(n)

  val unicodeEscape = "\\u" -~ hexDigit >> hex >> hex >> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> octal >> octal ^^ { _.asInstanceOf[Char] }
 
  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ item
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val parentheses = choice("()[]{}")
  val delimiter = choice("`'\".;,")
  val separator = parentheses | delimiter | whitespace
  
  val upper = choice("$_") | anyChar filter isUpperCase
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit | '$' | '_'
  val opChar = !choice(".()[]{}") -~ choice("!#%&*+-/:<=>?@\\^|~") | unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL)
  
  val op = opChar+
  val varid = lower ~++ idrest
  val plainid = upper ~++ idrest | varid | op
  val quotedid = delimit(printableChar+, '`')
  val id = (plainid | quotedid) ^^ literal

  lazy val idrest : Rule[List[Char]] = !idChar ^^^ Nil | ('_' ~++ op) ~- !idChar | idChar ~++ idrest

  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' ^^^ 0
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?) ~- !idChar
  
  val intPart = decimalNumeral?
  val floatPart = ('.'?) -~ (range('0', '9')*)
  val optSign = '+' ^^^ 1 | '-' ^^^ -1 | success(1)
  val exponentPart = choice("eE") -~ optSign ~ decimalNumeral ^^ { case sign ~ number => sign * number} ?
  val floatType = choice("fFdD") ?
   
    
  val floatingPointLiteral = (intPart ~ floatPart ~ exponentPart ~ floatType) filter {
    case None ~ Nil ~ _ ~ _ => false        // need at least one of intPart and FloatPart
    case _ ~ Nil ~ None ~ None => false  // need at least one of floatPart, exponentPart and floatType
    case _ => true
  } /* ^^ { case intDigits ~ floatDigits ~ exponent ~ suffix => 
      intDigits.getOrElse(0) + 
      floatDigits.mkString(".", "", "e") + 
      exponent.getOrElse(0) +
      suffix.getOrElse('d')
  } */

  
  val booleanLiteral = "true" | "false"

  val charEscapeSeq = '\\' ~- ( choice("\"\'\\")
      | 'b' ^^^ '\b'
      | 't' ^^^ '\t'
      | 'n' ^^^ '\n'
      | 'f' ^^^ '\f'
      | 'r' ^^^ '\r') 

  val charElement = unicodeEscape | octalEscape | charEscapeSeq | printableChar
  val characterLiteral = delimit(charElement, '\'')
  val stringLiteral = delimitN(charElement, '\"') | delimitN(anyChar, "\"\"\"")
  val symbolLiteral = '\'' ~ plainid
  
  
  val semi = (";" | newline) ~- (newline*)
   
  val operator = (opChar+) ^^ literal
  val special = choice(".()[]{}")
  val keyword = ("package" | "import" | "object" | "extends") ~- !idChar
  val other = (item filter { ch => !(ch isWhitespace) } +) ^^ literal

  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = bracketN("/*", multiLineComment | anyChar, "*/") ^^ literal
  val lineComment : Rule[String] = bracketN("//", item, newline.unary_&) ^^ literal
  val comment = lineComment | multiLineComment
}

abstract class IncrementalScalaScanner extends ScalaScanner with IncrementalScanner {
  val token = memo("token", (whitespace?) -~ (special | keyword | stringLiteral | comment | id | other))
  val tokens = view(token) _

  val line = memo("line", newline ^^^ "" | (!newline -~ item +) ~- (newline?) ^^ literal)
  val lines = view(line) _
}

object TestScalaScanner extends ScalaScanner with Application {
  type Context = ArrayInput[Char]
  
  def check[A](actual : Result[A], expected : Result[A]) {
    (expected, actual) match {
      case (Success(ea, es), Success(aa, as)) if ea == aa && es.toString == as.toString => ()
      case (e, a) if e == a => ()
      case _ => error ("Expected: " + expected + "\nActual: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[A])(input : String *) {
    for (i <- input) check(rule(i), Failure[Context])
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, Result[A]) *) {
    for ((input, result) <- expect) check(rule(input), result)
  }
  
  check(unicodeEscape("\\u0030"), Success('0', ""))
  check(octalEscape("\\061"), Success('1', ""))
  
  implicit def anyToSuccess[A](a : A) : Result[A] = Success(a, "")
  
  implicit def tripleToSuccess[A](triple : ((String, A), String)) : (String, Result[A]) = 
    triple match { case ((input, a), rest) => input -> Success(a, rest) }
  
  checkFailure(integerLiteral)("l", "L", "0x")
  
  checkRule(integerLiteral) (
      "0l" -> 0,
      "12 " -> 12 -> " ",
      "012" -> 10,
      "0x12" -> 18
      )
      
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