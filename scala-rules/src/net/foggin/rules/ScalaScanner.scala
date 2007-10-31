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
  val opChar = choice("!#%&*+-/:<=>?@\\^|~") | unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL)
  
  val op = opChar+
  val varid = lower ~++ idrest
  val plainid = upper ~++ idrest | varid | op
  val quotedid = delimit_+(printableChar, '`')
  val id = (plainid | quotedid) ^^ literal

  lazy val idrest : Rule[List[Char]] = !idChar ^^^ Nil | ('_' ~++ op) ~- !idChar | idChar ~++ idrest

  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' ^^^ 0
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?) ~- !idChar
  
  val intPart = decimalNumeral ^^ (_ toString)
  val floatPart = '.' ~++ (range('0', '9')*) ^^ literal
  val mantissa = atLeastOneOf(intPart, floatPart)
  val optSign = choice("+-") ^^ (_ toString) | ""
  val exponentPart = (choice("eE") ~ append(optSign, intPart)) ^^ { case e ~ n => e + n }
  val mantissaOptExponent = append(mantissa, exponentPart | "")
  
  def append(a : Rule[String], b : Rule[String]) = for (x <- a; y <- b) yield x + y
  def atLeastOneOf(a : Rule[String], b : Rule[String]) = append(a, b) | a | b
      
  val floatLiteral = mantissaOptExponent ~- choice("fF")
  val doubleLiteral = mantissaOptExponent ~- choice("dD") |
    append(intPart | "", atLeastOneOf(floatPart, exponentPart))
      
  
  val booleanLiteral = "true" | "false"

  val charEscapeSeq = '\\' ~- ( choice("\"\'\\")
      | 'b' ^^^ '\b'
      | 't' ^^^ '\t'
      | 'n' ^^^ '\n'
      | 'f' ^^^ '\f'
      | 'r' ^^^ '\r') 

  val charElement = unicodeEscape | octalEscape | charEscapeSeq | printableChar
  val characterLiteral = delimit(charElement, '\'')
  val stringLiteral = (delimit_*(charElement, '\"') | delimit_*(anyChar, "\"\"\"")) ^^ literal
  val symbolLiteral = '\'' ~ plainid
  
  
  val semi = (";" | newline) ~- (newline*)
   
  //val keyword = ("package" | "import" | "object" | "extends") ~- !idChar
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
  
  checkRule(unicodeEscape)("\\u0030" -> '0')
  checkRule(octalEscape)("\\061" -> '1')
  
  checkFailure(integerLiteral)("l", "L", "0x")
  
  checkRule(integerLiteral) (
      "0l" -> 0,
      "12 " -> 12 -> " ",
      "012" -> 10,
      "0x12" -> 18
      )
      
  checkFailure(opChar)(".", ";", "(", "[", "}")
  
  checkRule(opChar) (
      "+" -> '+',
      "-" -> '-',
      "*" -> '*',
      "/" -> '/')
   
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