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
  val quotedid = '`' -~ (printableChar+) ~- '`'
  val id = (plainid | quotedid) ^^ literal

  lazy val idrest : Rule[List[Char]] = ('_' ~++ op) | idChar ~++ idrest | success(Nil)

  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' ^^^ 0
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?)
  
  val intPart = decimalNumeral?
  val floatPart = ('.'?) -~ (range('0', '9')*)
  val optSign = '+' ^^^ 1 | '-' ^^^ -1 | success(1)
  val exponentPart = choice("eE") -~ optSign ~ decimalNumeral ^^ { case sign ~ number => sign * number} ?
  val floatType = choice("fFdD") ?
      
  val floatingPointLiteral = (intPart ~ floatPart ~ exponentPart ~ floatType) filter {
    case None ~ Nil ~ _ ~ _ => false
    case _ ~ Nil ~ None ~ None => false
    case _ => true
  }

  val booleanLiteral = "true" | "false"

  val charEscapeSeq = 
      "\\b" ^^^ '\b' | 
      "\\t" ^^^ '\t' | 
      "\\n" ^^^ '\n' | 
      "\\f" ^^^ '\f' | 
      "\\r" ^^^ '\r' | 
      "\\\"" ^^^ '\"' | 
      "\\\'" ^^^ '\'' | 
      "\\\\" ^^^ '\\'

  val characterLiteral = '\'' -~ (charEscapeSeq | !choice("\'\\") -~ printableChar) ~- '\''
  val stringLiteral = 
      '\"' -~ ((charEscapeSeq | !choice("\"\\") -~ printableChar)*) ~- '\"' ^^ literal |
      "\"\"\"" -~ ((!"\"\"\"" -~ anyChar)*) ~- "\"\"\"" ^^ literal
  val symbolLiteral = '\'' ~ plainid
  
  val lineComment = "//" -~ ((!newline -~ item)*)
  
  val semi = (";" | newline) ~- (newline*)
   
  val operator = (opChar+) ^^ literal
  val special = choice(".()[]{}")
  val keyword = ("package" | "import" | "object" | "extends") ~- !(anyChar filter isJavaIdentifierPart)
  val other = (item filter { ch => !(ch isWhitespace) } +) ^^ literal
  //val stringLiteral = '"' -~ (item filter (_ != '"')*) ~- '"' ^^ literal

  // note multi-line comments can nest
  lazy val comment : Rule[String] = "/*" -~ ((!"*/") -~ (comment | anyChar)).* ~- "*/" ^^ literal

}

abstract class IncrementalScalaScanner extends ScalaScanner with IncrementalScanner {
  val token = memo("token", (whitespace?) -~ (special | keyword | stringLiteral | comment | id | other))
  val tokens = view(token) _

  val line = memo("line", newline ^^^ "" | (!newline -~ item +) ~- (newline?) ^^ literal)
  val lines = view(line) _
}

object TestScalaScanner extends IncrementalScalaScanner with Application {
  var input = new EditableInput[Char]

  //println(unicodeEscape("\\u0030"))
  //println(octalEscape("\\060"))

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